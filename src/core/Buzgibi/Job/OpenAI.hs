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

module Buzgibi.Job.OpenAI (performSentimentalAnalysis, OpenAICfg (..)) where


import Buzgibi.Statement.User.Survey 
       ( getSurveysForSA, 
        insertSA,
        setInsufficientFund,
        Status (TranscriptionsDoneOpenAI),
        OpenAISA (..)
       )
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Buzgibi.Api.CallApi
import Buzgibi.Transport.Model.OpenAI 
import Buzgibi.EnvKeys (OpenAI (..))
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
import Control.Monad.IO.Class
import Data.String.Conv (toS)
import Data.Bifunctor (bimap)
import BuildInfo (location)
import Data.Either.Combinators (whenLeft)
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

type instance Api "completions" SARequest SAResponse = ()

mkErrorMsg _ _ _ [] = pure ()
mkErrorMsg job logger surveyIdent ((phoneIdent, e):es) = 
  logger ErrorS $ logStr $ $location <> "(" <> job <> "): survey id: " <> show surveyIdent <> ", phone id: " <> show phoneIdent <> ", error ---> " <> toS e

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
                (Left (Just request)) methodPost [] mempty Left $
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