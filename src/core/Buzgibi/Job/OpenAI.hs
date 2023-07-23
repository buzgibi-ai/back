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

module Buzgibi.Job.OpenAI (getTranscription, OpenAIEnv (..)) where


import Buzgibi.Statement.User.Survey (getSurveysForTranscription, insertTranscription, OpenAIPhone (..))
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
import Control.Monad (when)
import Data.Either (isLeft)
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

data OpenAIEnv =
     OpenAIEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       openaiCfg :: OpenAI,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn
     }

type instance Api "audio/transcriptions" () TranscriptionResponse = ()

getTranscription :: OpenAIEnv -> IO ()
getTranscription OpenAIEnv {..} = forever $ do 
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.OpenAI: start at " <> show start

  xs <- transaction pool logger $ statement getSurveysForTranscription ()
  Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
    let phones = sequence $ map (eitherDecode @OpenAIPhone . encode) ys
    res <- for phones $ \xs -> do
      yse <- Async.forConcurrently xs $
        \OpenAIPhone {..} -> 
          fmap (bimap (openAIPhonePhoneIdent,) (openAIPhonePhoneIdent,) . join . first (toS . show)) $ 
          Minio.runMinioWith minio $ do
            let (ext:_) = openAIPhoneVoiceExt
            tm <- (toS . show . systemSeconds) <$> liftIO getSystemTime
            o <- Minio.getObject openAIPhoneVoiceBucket openAIPhoneVoiceHash Minio.defaultGetObjectOptions
            path <-
              runConduit $
               Minio.gorObjectStream o
                 .| sinkSystemTempFile
                 (toS (openAIPhoneVoiceTitle <> "_" <> tm <> "." <> ext))
            let parts = [partFileSource "file" path, partBS "model" "whisper-1"]    
            liftIO $ callApi @"audio/transcriptions" @() @TranscriptionResponse
              (ApiCfg openaiCfg manager logger) (Right parts) methodPost mempty Left $ 
                (pure . transcriptionResponseText . fst)

      let (es, ys) = partitionEithers yse
      mkErrorMsg logger survIdent es
      transaction pool logger $ statement insertTranscription (survIdent, ys)
    when (isLeft res) $ logger ErrorS $ logStr $ "Buzgibi.Job.OpenAI: phone parse failed for survey " <> show survIdent

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.OpenAI: end at " <> show end

mkErrorMsg _ _ [] = pure ()
mkErrorMsg logger surveyIdent ((phoneIdent, e):es) = 
  logger ErrorS $ logStr $ "Buzgibi.Job.OpenAI: survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e 