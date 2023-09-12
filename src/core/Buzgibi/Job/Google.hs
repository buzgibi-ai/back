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

module Buzgibi.Job.Google (transcribeVoice, GoogleCfg (..)) where

import Buzgibi.Statement.User.Survey 
       (VoiceForTranscription (..), 
        checkAfterTranscription, 
        insertTranscription,
        mkTranscriptionOk,
        mkTranscriptionFailure,
        getSurveysForTranscription)
import Buzgibi.Job.Google.Token ()        
import Buzgibi.Transport.Model.Google
import Buzgibi.EnvKeys (clarifyingPrefix)
import Buzgibi.Job.Google.Api (callApi, Api)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Hasql.Connection as Hasql
import Buzgibi.EnvKeys (Google)
import Buzgibi.Job.Utils (withElapsedTime)
import Data.Pool (Pool)
import Katip
import qualified Network.Minio as Minio
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import BuildInfo (location)
import Data.Aeson (eitherDecode, encode)
import qualified Control.Concurrent.Async as Async
import Database.Transaction
import Control.Monad.IO.Class
import Conduit (runConduit, sinkSystemTempFile, (.|))
import Data.String.Conv (toS)
import Data.Bifunctor (first, bimap, second)
import Control.Monad (join)
import Control.Lens ((^.))
import Data.Either.Combinators (whenLeft)
import Data.Either (partitionEithers)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Traversable (for)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Request (forConcurrentlyNRetry)
import qualified Data.Text as T
import Data.Int (Int64)
import System.Process (readProcess)


data GoogleCfg =
     GoogleCfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       googleCfg :: Google,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn,
       jobFrequency :: Int
     }

type instance Api "speech:recognize" TranscribeVoiceRequest TranscribeVoiceResponse = ()

mkErrorMsg _ _ _ [] = pure ()
mkErrorMsg job logger surveyIdent ((phoneIdent, e):es) = do
  void $ logger ErrorS $ logStr $ $location <> "(" <> job <> "): survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e
  mkErrorMsg job logger surveyIdent es

transcribeVoice :: GoogleCfg -> IO ()
transcribeVoice GoogleCfg {..} = forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(getTranscription)") $ do

      xs <- transaction pool logger $ statement getSurveysForTranscription ()
      Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
        let phones = sequence $ map (eitherDecode @VoiceForTranscription . encode) ys
        res <- for phones $ \xs -> do
          yse <- forConcurrentlyNRetry 5 2 retryTranscription xs $
            \VoiceForTranscription {..} ->
              fmap (bimap (voiceForTranscriptionPhoneIdent,) (voiceForTranscriptionPhoneIdent,) . join . first (toS . show)) $ 
              Minio.runMinioWith minio $ do
                let (ext:_) = voiceForTranscriptionVoiceExts
                tm <- (toS . show . systemSeconds) <$> liftIO getSystemTime
                o <- Minio.getObject voiceForTranscriptionVoiceBucket voiceForTranscriptionVoiceHash Minio.defaultGetObjectOptions
                path <-
                  runConduit $
                  Minio.gorObjectStream o
                    .| sinkSystemTempFile
                    (toS (voiceForTranscriptionVoiceTitle <> "_" <> tm <> "." <> ext))
                duration <- liftIO $ readProcess "buzgibi-audio-file-duration" [path] mempty
                if read @Double duration > 60 then 
                  pure $ Right mempty
                else do  
                  voice <- liftIO $ fmap (decodeUtf8 . B64.encode) $ B.readFile path
                  let request = TranscribeVoiceRequest defConfig $ Audio voice
                  resp <- liftIO $ callApi @"speech:recognize" @TranscribeVoiceRequest @TranscribeVoiceResponse googleCfg manager HTTP.methodPost request
                  for resp $ \TranscribeVoiceResponse{..} ->
                    pure $ fromMaybe mempty $ fmap gleanTranscription transcribeVoiceResponseResults
 
          let (es, ys) = partitionEithers yse
          mkErrorMsg "getTranscription" logger survIdent es
          let xs = map (second (mkTranscriptionOk (googleCfg^.clarifyingPrefix))) ys <> map (second mkTranscriptionFailure) es
          transaction pool logger $ statement insertTranscription (survIdent, xs) *> statement checkAfterTranscription survIdent
    
        whenLeft res $ \error ->
          logger CriticalS $ logStr $ $location <> ": phone parse failed for survey " <> show survIdent <> ", error: " <> error

gleanTranscription :: [Result] -> T.Text
gleanTranscription [] = mempty
gleanTranscription ((Result []):ys) = mempty <> gleanTranscription ys
gleanTranscription ((Result xs):ys) = alternativesTranscript (head (sortBy (comparing alternativesConfidence) xs)) <> gleanTranscription ys

retryTranscription :: Either (Int64, T.Text) (Int64, T.Text) -> IO Bool
retryTranscription (Left _) = pure True
retryTranscription (Right _) = pure False