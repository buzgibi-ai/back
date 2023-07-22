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

module Buzgibi.Job.Telnyx (makeApp, makeCall, TelnyxEnv (..)) where

import Buzgibi.Statement.User.Survey 
       (getSurveyForTelnyxApp, 
        insertTelnyxApp, 
        getPhonesToCall, 
        insertAppCall)
import Buzgibi.EnvKeys (Telnyx (..))
import Buzgibi.Transport.Model.Telnyx
import Buzgibi.Api.Telnyx
import Database.Transaction
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import Data.Time.Clock (getCurrentTime)
import qualified Control.Concurrent.Async as Async
import qualified Network.HTTP.Client as HTTP
import Data.String.Conv
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Coerce (coerce)

data TelnyxEnv =
     TelnyxEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       telnyxCfg :: Telnyx,
       manager :: HTTP.Manager
     }

type instance TelnyxApi "call_control_applications" AppRequest AppResponse = ()
type instance TelnyxApi "calls" CallRequest CallResponseData = ()

makeApp :: TelnyxEnv -> IO ()
makeApp TelnyxEnv {..} = forever $ do 
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Telnyx: start at " <> show start
  xs <- transaction pool logger $ statement getSurveyForTelnyxApp ()
  logger DebugS $ logStr $ "Buzgibi.Job.Telnyx: surveys for Telnyx " <> show xs

  resp <- Async.forConcurrently xs $ \(ident, title) -> do 
    logger DebugS $ logStr $ "Buzgibi.Job.Telnyx: trying creating app for " <> show ident
    let webhook = "https://buzgibi.app/foreign/webhook/telnyx"
    let request =
          AppRequest 
          { appRequestApplicationName = title,
            appRequestWebhookEventUrl = webhook
          }
    callApi @"call_control_applications" @AppRequest @AppResponse (TelnyxApiCfg telnyxCfg manager logger) request methodPost mempty (Left . (ident, )) $ \(app, _) -> pure $ (ident, title,) $ coerce app

  let (errXs, appXs) = partitionEithers resp
  for_ errXs $ \(ident, e) -> logger ErrorS $ logStr $ " app for " <> show ident <> " hasn't been created, error --> " <> toS e
  
  logger InfoS $ logStr $ "apps for the following surveys " <> show appXs <> " are about to be added"
  for_ appXs $ transaction pool logger . statement insertTelnyxApp

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Telnyx: end at " <> show end

makeCall :: TelnyxEnv -> IO ()
makeCall TelnyxEnv {..} = forever $ do
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Telnyx: start at " <> show start

  xs <- transaction pool logger $ statement getPhonesToCall ()
  
  resp <- Async.forConcurrently xs $ \(ident, telnyxIdent, link, phones) -> do 
    let request =
          CallRequest
          {
            callRequestTo = phones,
            callRequestFrom = telnyxPhone telnyxCfg,
            callRequestFromDisplayName = mempty,
            callRequestConnectionId = telnyxIdent,
            callRequestAudioUrl = link
          }
    callApi @"calls" @CallRequest @CallResponseData (TelnyxApiCfg telnyxCfg manager logger) request methodPost mempty (Left . (ident, )) $ \(call, _) -> pure (ident, coerce call)    
 
  let (errXs, callXs) = partitionEithers resp
  for_ errXs $ \(ident, e) -> logger ErrorS $ logStr $ " call for " <> show ident <> " hasn't been made, error --> " <> toS e  

  for_ callXs $ transaction pool logger . statement insertAppCall

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Telnyx: end at " <> show end
