{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Buzgibi.Job.Telnyx (makeApp, TelnyxEnv (..)) where

import Buzgibi.Statement.User.Survey (getSurveyForTelnyxApp, insertTelnyxApp)
import Buzgibi.EnvKeys (Telnyx (..))
import Buzgibi.Transport.Model.Telnyx
import Database.Transaction
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import Data.Time.Clock (getCurrentTime)
import qualified Control.Concurrent.Async as Async
import Control.Exception (try)
import Network.HTTP.Client (HttpException)
import Data.Bifunctor (first)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (methodPost, hAuthorization, hContentType)
import Data.String.Conv
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Coerce (coerce)
import qualified Request as Request

data TelnyxEnv =
     TelnyxEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       telnyxCfg :: Telnyx,
       manager :: HTTP.Manager
     }

makeApp :: TelnyxEnv -> IO ()
makeApp TelnyxEnv {..} = forever $ do 
  threadDelay (5 * 10 ^ 6)
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
    let url = telnyxUrl telnyxCfg <> "/call_control_applications"
    let authH = (hAuthorization, toS ("Bearer " <> telnyxKey telnyxCfg))
    let contTypeH = (hContentType, "application/json")
    resp <- fmap (join . first (toS . show)) $ try @HttpException $ Request.make url manager [authH, contTypeH] methodPost $ Just $ request
    return $ Request.withError @AppResponse resp (Left . (ident, )) $ \(app, _) -> pure $ (ident, title,) $ coerce app

  let (errXs, appXs) = partitionEithers resp
  for_ errXs $ \(ident, e) -> logger ErrorS $ logStr $ " app for " <> show ident <> " hasn't been created, error --> " <> toS e
  
  logger InfoS $ logStr $ "apps for the following surveys " <> show appXs <> " are about to be added"
  for_ appXs $ transaction pool logger . statement insertTelnyxApp

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Telnyx: end at " <> show end



