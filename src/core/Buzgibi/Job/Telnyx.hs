{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Buzgibi.Job.Telnyx (makeApp, makeCall, TelnyxCfg (..)) where

import Buzgibi.Statement.User.Survey 
       (getSurveyForTelnyxApp, 
        insertTelnyxApp, 
        getPhonesToCall, 
        insertAppCall,
        invalidatePhones,
        checkAfterInvalidate,
        failTelnyxApp,
        setInsufficientFund,
        PhoneToCall (..),
        Status (PickedByTelnyx))
import Buzgibi.Api.CallApi.Instance ()        
import Buzgibi.EnvKeys (Telnyx (..))
import Buzgibi.Transport.Model.Telnyx
import Buzgibi.Api.CallApi
import Buzgibi.Job.Utils (withElapsedTime)
import Database.Transaction
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import qualified Control.Concurrent.Async as Async
import qualified Network.HTTP.Client as HTTP
import Data.String.Conv
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Coerce (coerce)
import BuildInfo (location)
import qualified Data.Text as T
import Data.Aeson (eitherDecode, encode, decode)
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)
import Data.Tuple.Extended (consT)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

data TelnyxCfg =
     TelnyxCfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       telnyxCfg :: Telnyx,
       manager :: HTTP.Manager,
       webhook :: T.Text,
       jobFrequency :: Int
     }

type instance Api "call_control_applications" AppRequest AppResponse = ()
type instance Api "calls" CallRequest CallResponseData = ()

makeApp :: TelnyxCfg -> IO ()
makeApp TelnyxCfg {..} = forever $ do 
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(makeApp)") $ do

    xs <- transaction pool logger $ statement getSurveyForTelnyxApp ()
    logger DebugS $ logStr $ $location <>"(makeApp): surveys for Telnyx " <> show xs

    resp <- Async.forConcurrently xs $ \(ident, title) -> do 
      logger DebugS $ logStr $ $location <> "(makeApp): trying creating app for " <> show ident
      let url = webhook <> "/foreign/webhook/telnyx"
      let request =
            AppRequest 
            { appRequestApplicationName = title <> "_" <> telnyxApppostfix telnyxCfg,
              appRequestWebhookEventUrl = url,
              appRequestOutbound = 
              Outbound {
                outboundChannelLimit = Just $ (length xs) + 10,
                outboundOutboundVoiceProfileId = telnyxOutbound telnyxCfg
              }
            }
      callApi @"call_control_applications" @AppRequest @AppResponse 
        (ApiCfg telnyxCfg manager logger) (Left (Just request)) methodPost mempty (Left . (ident, )) $ 
          \(app, _) -> pure $ (ident, title,) $ coerce app

    let (errXs, appXs) = partitionEithers resp
    for_ errXs $ \(ident, e) -> logger ErrorS $ logStr $ $location <> " app for " <> show ident <> " hasn't been created, error --> " <> toS e
    transaction pool logger $ statement failTelnyxApp $ errXs
    
    logger InfoS $ logStr $ $location <> "apps for the following surveys " <> show appXs <> " are about to be added"
    for_ appXs $ transaction pool logger . statement insertTelnyxApp


makeCall :: TelnyxCfg -> IO ()
makeCall TelnyxCfg {..} = forever $ do
  threadDelay (jobFrequency * 10 ^ 6)
  withElapsedTime logger ($location <> "(makeCall)") $ do

    xs <- transaction pool logger $ statement getPhonesToCall ()
    
    Async.forConcurrently_ xs $ \(ident, telnyxIdent, link, phonesJson) -> do
      let phonese = sequence $ map (eitherDecode @PhoneToCall . encode) phonesJson
      decodeRes <- for phonese $ \phones -> do 
        resp <- Async.forConcurrently phones $ \PhoneToCall {..} -> do

          let request =
                CallRequest
                {
                  callRequestTo = [phoneToCallPhone],
                  callRequestFrom = telnyxPhone telnyxCfg,
                  callRequestFromDisplayName = mempty,
                  callRequestConnectionId = telnyxIdent,
                  -- there is a wrong place for an audio to be played back
                  -- audio is being played prematurely and by 
                  -- the moment a callee has picked up a phone it's finished
                  -- The solution is to move it to `answer webhook` 
                  -- and query https://developers.telnyx.com/openapi/callcontrol/tag/Call-Commands/#tag/Call-Commands/operation/callPlaybackStart
                  callRequestAudioUrl = mempty,
                  callRequestAnsweringMachineDetection = telnyxMachine telnyxCfg
                }
          callApi @"calls" @CallRequest @CallResponseData 
            (ApiCfg telnyxCfg manager logger) (Left (Just request)) methodPost mempty (Left . (phoneToCallIdent,)) $ 
              \(call, _) -> pure $ consT phoneToCallIdent $ encodeCallResponse (coerce call)
  
        let (errXs, callXs) = partitionEithers resp
        if ifInsufficientFunds errXs
        then do
          transaction pool logger $ statement setInsufficientFund (ident, PickedByTelnyx)
          logger EmergencyS $ logStr @String $ $location <> " the service cannot function normally due to the lack of funds"
        else do
          for_ errXs $ \e -> logger ErrorS $ logStr $ $location <> " call for " <> show ident <> " hasn't been made, error --> " <> toS (show e)
          transaction pool logger $ statement insertAppCall callXs
          surveyId <- transaction pool logger $ statement invalidatePhones errXs
          for_ surveyId $ transaction pool logger . statement checkAfterInvalidate

      whenLeft decodeRes $ \e -> logger CriticalS $ logStr $ $location <> " call for " <> show ident <> " hasn't been made, error --> " <> toS (show e)

ifInsufficientFunds :: [(Int64, T.Text)] -> Bool
ifInsufficientFunds = or . map (fromMaybe False . fmap (go . coerce) . (decode @Errors) . toS . snd)
  where
    go [] = False 
    go (Error {errorCode}:xs) 
      | errorCode == 20100 = True
      | otherwise = go xs