{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Job.OpenAI (getTranscription, performSentimentalAnalysis, OpenAICfg (..)) where


import Buzgibi.Statement.User.Survey 
       (getSurveysForTranscription, 
        getSurveysForSA, 
        insertTranscription,
        insertSA,
        OpenAITranscription (..), 
        OpenAISA (..)
       )
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Buzgibi.Api.CallApi
import Buzgibi.Transport.Model.OpenAI 
import Buzgibi.EnvKeys (OpenAI (..))
import Buzgibi.Api.CallApi.Instance () 
import Data.Pool (Pool)
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Database.Transaction
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import Data.Aeson (eitherDecode, encode)
import qualified Control.Concurrent.Async as Async
import qualified Network.Minio as Minio
import Data.Either (partitionEithers)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad.IO.Class
import Conduit (runConduit, sinkSystemTempFile, (.|))
import Data.String.Conv (toS)
import Network.HTTP.Client.MultipartFormData (partBS, partFileSource)
import Data.Bifunctor (first, bimap)
import Control.Monad (join)
import BuildInfo (location)
import Data.Either.Combinators (whenLeft)

data OpenAICfg =
     OpenAICfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       openaiCfg :: OpenAI,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn
     }

type instance Api "audio/transcriptions" () TranscriptionResponse = ()
type instance Api "audio/completions" SARequest SAResponse = ()

mkErrorMsg _ _ _ [] = pure ()
mkErrorMsg job logger surveyIdent ((phoneIdent, e):es) = 
  logger ErrorS $ logStr $ $location <> "(" <> job <> "): survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e

getTranscription :: OpenAICfg -> IO ()
getTranscription OpenAICfg {..} = forever $ do 
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ $location <> "(getTranscription): start at " <> show start

  xs <- transaction pool logger $ statement getSurveysForTranscription ()
  Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
    let phones = sequence $ map (eitherDecode @OpenAITranscription . encode) ys
    res <- for phones $ \xs -> do
      yse <- Async.forConcurrently xs $
        \OpenAITranscription {..} -> 
          fmap (bimap (openAITranscriptionPhoneIdent,) (openAITranscriptionPhoneIdent,) . join . first (toS . show)) $ 
          Minio.runMinioWith minio $ do
            let (ext:_) = openAITranscriptionVoiceExt
            tm <- (toS . show . systemSeconds) <$> liftIO getSystemTime
            o <- Minio.getObject openAITranscriptionVoiceBucket openAITranscriptionVoiceHash Minio.defaultGetObjectOptions
            path <-
              runConduit $
               Minio.gorObjectStream o
                 .| sinkSystemTempFile
                 (toS (openAITranscriptionVoiceTitle <> "_" <> tm <> "." <> ext))
            let parts = [partFileSource "file" path, partBS "model" "whisper-1"]    
            liftIO $ callApi @"audio/transcriptions" @() @TranscriptionResponse
              (ApiCfg openaiCfg manager logger) (Right parts) methodPost mempty Left $ 
                (pure . transcriptionResponseText . fst)

      let (es, ys) = partitionEithers yse
      mkErrorMsg "getTranscription" logger survIdent es
      transaction pool logger $ statement insertTranscription (survIdent, ys)
    whenLeft res $ \error ->  
      logger ErrorS $ logStr $ $location <> ": phone parse failed for survey " <> show survIdent <> ", error: " <> error

  end <- getCurrentTime
  logger InfoS $ logStr $ $location <> "(getTranscription): end at " <> show end

performSentimentalAnalysis :: OpenAICfg -> IO ()
performSentimentalAnalysis OpenAICfg {..} = 
  forever $ do 
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ $location <> "(performSentimentalAnalysis): start at " <> show start

  xs <- transaction pool logger $ statement getSurveysForSA () 
  Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
    let phones = sequence $ map (eitherDecode @OpenAISA . encode) ys
    res <- for phones $ \xs -> do 
      yse <- Async.forConcurrently xs $
        \OpenAISA {..} -> liftIO $ do 
           let request = defSARequest { sARequestPrompt = openAISAText }
           fmap (bimap (openAISAPhoneIdent,) (openAISAPhoneIdent,)) $
             callApi @"audio/completions" @SARequest @SAResponse 
               (ApiCfg openaiCfg manager logger) 
               (Left request) methodPost mempty Left $
                 \(SAResponse xs, _) ->
                   case xs of [] -> Left "empty choices"; (x:_) -> Right x

      let (es, ys) = partitionEithers yse
      mkErrorMsg "performSentimentalAnalysis" logger survIdent es
      transaction pool logger $ statement insertSA (survIdent, ys)

    whenLeft res $ \error -> 
      logger ErrorS $ logStr $ $location <> ": SA failed for survey " <> show survIdent <> ", error: " <> error

  end <- getCurrentTime
  logger InfoS $ logStr $ $location <> "(performSentimentalAnalysis): end at " <> show end