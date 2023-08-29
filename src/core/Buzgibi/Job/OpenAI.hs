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
        mkTranscriptionOk,
        mkTranscriptionFailure,
        insertSA,
        checkAfterTranscription,
        setInsufficientFund,
        Status (TranscriptionsDoneOpenAI, ProcessedByTelnyx),
        OpenAITranscription (..), 
        OpenAISA (..)
       )
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Buzgibi.Api.CallApi
import Buzgibi.Transport.Model.OpenAI 
import Buzgibi.EnvKeys (OpenAI (..), clarifyingPrefix)
import Buzgibi.Job.Utils (withElapsedTime)
import Buzgibi.Api.CallApi.Instance () 
import Data.Pool (Pool)
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Database.Transaction
import Data.Traversable (for)
import Data.Aeson (eitherDecode, encode, decode)
import qualified Control.Concurrent.Async as Async
import qualified Network.Minio as Minio
import Data.Either (partitionEithers)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad.IO.Class
import Conduit (runConduit, sinkSystemTempFile, (.|))
import Data.String.Conv (toS)
import Network.HTTP.Client.MultipartFormData (partBS, partFileSource)
import Data.Bifunctor (first, bimap, second)
import Control.Monad (join)
import BuildInfo (location)
import Data.Either.Combinators (whenLeft)
import Control.Lens ((^.))
import Request (forConcurrentlyNRetry)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Maybe (fromMaybe)


data OpenAICfg =
     OpenAICfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       openaiCfg :: OpenAI,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn,
       jobFrequency :: Int
     }

type instance Api "audio/transcriptions" () TranscriptionResponse = ()
type instance Api "completions" SARequest SAResponse = ()

mkErrorMsg _ _ _ [] = pure ()
mkErrorMsg job logger surveyIdent ((phoneIdent, e):es) = 
  logger ErrorS $ logStr $ $location <> "(" <> job <> "): survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e

getTranscription :: OpenAICfg -> IO ()
getTranscription OpenAICfg {..} = forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(getTranscription)") $ do

    xs <- transaction pool logger $ statement getSurveysForTranscription ()
    Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
      let phones = sequence $ map (eitherDecode @OpenAITranscription . encode) ys
      res <- for phones $ \xs -> do
        yse <- forConcurrentlyNRetry 3 60 retryTranscription xs $
          \OpenAITranscription {..} ->
            if openAITranscriptionAttempts < 3 then
              fmap (bimap (openAITranscriptionPhoneIdent,) (openAITranscriptionPhoneIdent,) . join . first (toS . show)) $ 
              Minio.runMinioWith minio $ do
                let (ext:_) = openAITranscriptionVoiceExts
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
            else pure $ Right (openAITranscriptionPhoneIdent, mempty)

        let (es, ys) = partitionEithers yse
        if ifInsufficientFunds es
        then do
          transaction pool logger $ statement setInsufficientFund (survIdent, ProcessedByTelnyx)
          logger EmergencyS $ logStr @String $ $location <> " the service cannot function normally due to the lack of funds"
        else do
          mkErrorMsg "getTranscription" logger survIdent es
          let xs = map (second (mkTranscriptionOk (openaiCfg^.clarifyingPrefix))) ys <> map (second mkTranscriptionFailure) es
          transaction pool logger $ statement insertTranscription (survIdent, xs) *> statement checkAfterTranscription survIdent
  
      whenLeft res $ \error ->  
        logger CriticalS $ logStr $ $location <> ": phone parse failed for survey " <> show survIdent <> ", error: " <> error

retryTranscription :: Either (Int64, T.Text) (Int64, T.Text) -> IO Bool
retryTranscription (Left _) = pure True
retryTranscription (Right _) = pure False

performSentimentalAnalysis :: OpenAICfg -> IO ()
performSentimentalAnalysis OpenAICfg {..} = 
  forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(performSentimentalAnalysis)") $ do

    xs <- transaction pool logger $ statement getSurveysForSA () 
    Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
      let phones = sequence $ map (eitherDecode @OpenAISA . encode) ys
      res <- for phones $ \xs -> do 
        yse <- Async.forConcurrently xs $
          \OpenAISA {..} -> liftIO $ do 
            let request = defSARequest { sARequestPrompt = openAISAText }
            fmap (bimap (openAISAPhoneIdent,) (openAISAPhoneIdent,)) $
              callApi @"completions" @SARequest @SAResponse 
                (ApiCfg openaiCfg manager logger) 
                (Left (Just request)) methodPost mempty Left $
                  \(SAResponse xs, _) ->
                    case xs of [] -> Left "empty choices"; (x:_) -> Right x

        let (es, ys) = partitionEithers yse
        if ifInsufficientFunds es
        then do
          transaction pool logger $ statement setInsufficientFund (survIdent, TranscriptionsDoneOpenAI)
          logger EmergencyS $ logStr @String $ $location <> " the service cannot function normally due to the lack of funds"
        else do
          mkErrorMsg "performSentimentalAnalysis" logger survIdent es
          transaction pool logger $ statement insertSA (survIdent, ys)

      whenLeft res $ \error -> 
        logger ErrorS $ logStr $ $location <> ": SA failed for survey " <> show survIdent <> ", error: " <> error

ifInsufficientFunds :: [(Int64, T.Text)] -> Bool
ifInsufficientFunds = or . map (fromMaybe False . fmap go . (decode @Error) . toS . snd)
  where go (Error e) = e == "insufficient_quota"