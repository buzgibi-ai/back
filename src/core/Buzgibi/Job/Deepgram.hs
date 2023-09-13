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
{-# LANGUAGE QuasiQuotes #-}

module Buzgibi.Job.Deepgram (transcribeVoice, DeepgramCfg (..)) where

import Buzgibi.Statement.User.Survey 
       (VoiceForTranscription (..), 
        checkAfterTranscription, 
        insertTranscription,
        mkTranscriptionOk,
        mkTranscriptionFailure,
        getSurveysForTranscription)
import Buzgibi.Job.Deepgram.Api (callApi)
import Buzgibi.Job.Utils (withElapsedTime)
import Buzgibi.EnvKeys (Deepgram, url, key, lang, clarifyingPrefix)
import Buzgibi.Transport.Model.Deepgram (Result (..), Alternatives (..))
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Katip
import Data.Pool (Pool)
import qualified Network.Minio as Minio
import Control.Monad (forever, join, void)
import BuildInfo (location)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Database.Transaction
import Data.Aeson (eitherDecode, encode)
import Data.Either.Combinators (whenLeft)
import Data.Traversable (for)
import Data.String.Conv (toS)
import Data.Bifunctor (first, bimap, second)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Either (partitionEithers)
import Data.Aeson (object, (.=))
import qualified Text.RE.PCRE.Text as Reg

data DeepgramCfg = 
     DeepgramCfg
    { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn,
       jobFrequency :: Int,
       deepgramCfg :: Deepgram
    }

mkErrorMsg _ _ _ [] = pure ()
mkErrorMsg job logger surveyIdent ((phoneIdent, e):es) = do
  void $ logger ErrorS $ logStr $ $location <> "(" <> job <> "): survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e
  mkErrorMsg job logger surveyIdent es

transcribeVoice :: DeepgramCfg -> IO ()
transcribeVoice DeepgramCfg {..} = forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(transcribeVoice)") $ do

    xs <- transaction pool logger $ statement getSurveysForTranscription ()
    Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
      let phones = sequence $ map (eitherDecode @VoiceForTranscription . encode) ys
      res <- for phones $ \xs -> do
        yse <- for xs $ \VoiceForTranscription {..} ->
          fmap (bimap (voiceForTranscriptionPhoneIdent,) (voiceForTranscriptionPhoneIdent,) . join . first (toS . show)) $ 
            Minio.runMinioWith minio $ do
              link <- Minio.presignedGetObjectUrl voiceForTranscriptionVoiceBucket voiceForTranscriptionVoiceHash 3600 mempty mempty
              let link' = toS link Reg.?=~/ [Reg.ed|^https?:\/\/[A-Za-z0-9:.]*///http://35.210.166.20|]
              let bs = encode $ object ["url" .= link' ]
              resp <- liftIO $ callApi @Result (deepgramCfg^.url <> deepgramCfg^.lang) (deepgramCfg^.key) manager $ toS bs
              for resp $ \(Result xs) -> 
                let r = sortBy (comparing alternativesConfidence) xs
                in pure $ case r of [] -> mempty; (x:_) -> alternativesTranscript x

        let (es, ys) = partitionEithers yse
        mkErrorMsg "transcribeVoice" logger survIdent es
        let xs = map (second (mkTranscriptionOk (deepgramCfg^.clarifyingPrefix))) ys <> map (second mkTranscriptionFailure) es
        transaction pool logger $ statement insertTranscription (survIdent, xs) *> statement checkAfterTranscription survIdent

      whenLeft res $ \error -> logger CriticalS $ logStr $ $location <> ": phone parse failed for survey " <> show survIdent <> ", error: " <> error