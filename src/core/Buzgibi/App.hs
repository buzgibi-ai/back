{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Buzgibi.App (Cfg (..), AppM (..), run) where

import BuildInfo
import Buzgibi.Job.Telnyx as Job.Telnyx
import Buzgibi.Api
import qualified Buzgibi.Api.Controller.Controller as Controller
import Buzgibi.AppM
import qualified Buzgibi.Config as Cfg
import Buzgibi.Transport.Error
import qualified Buzgibi.Transport.Response as Response
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict as RWS
import Data.Aeson
import Data.Bool
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Product.Fields
import Data.String.Conv
import qualified Data.Text as T
import Katip
import Katip.Controller
import Language.Haskell.TH.Syntax (Loc)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Header.Extended
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
-- import qualified Network.Wai.Middleware.Servant.Logger as Middleware
import Network.Wai.Parse
import Servant
import Servant.API.Generic
import Servant.Auth.Server
import Servant.Error.Formatters (formatters)
import Servant.Multipart
import Servant.Swagger.UI
import TextShow
import qualified Katip.Wai as Katip.Wai
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))

data Cfg = Cfg
  { cfgHost :: !String,
    cfgSwaggerPort :: !(Maybe Int),
    cfgServerPort :: !Int,
    cfgCors :: !Cfg.Cors,
    cfgServerError :: !Cfg.ServerError,
    mute500 :: !(Maybe Bool),
    ns :: !Namespace,
    logEnv :: !LogEnv
  }

run :: Cfg -> KatipContextT AppM ()
run Cfg {..} = katipAddNamespace (Namespace ["application"]) $ do
  logger <- katipAddNamespace (Namespace ["application"]) askLoggerIO

  version_e <- liftIO getVersion
  whenLeft version_e $ \e -> throwM $ ErrorCall e
  let Right ver = version_e

  $(logTM) DebugS $ ls $ "server run on: " <> "http://127.0.0.1:" <> showt cfgServerPort

  configKatipEnv <- lift ask
  let initCfg = do
        configEnv <- getLogEnv
        configCtx <- getKatipContext
        configNm <- getKatipNamespace
        return $ Config {..}
  cfg <- initCfg
  let withSwagger :: Proxy a -> Proxy (a :<|> SwaggerSchemaUI "swagger" "swagger.json")
      withSwagger _ = Proxy

  let server =
        hoistServerWithContext
          (withSwagger api)
          (Proxy @'[CookieSettings, JWTSettings])
          (runKatipController cfg (KatipControllerState 0))
          ( toServant Controller.controller
              :<|> swaggerSchemaUIServerT
                (swaggerHttpApi cfgHost cfgSwaggerPort ver)
          )
  excep <- katipAddNamespace (Namespace ["exception"]) askLoggerIO
  ctx_logger <- katipAddNamespace (Namespace ["context"]) askLoggerIO
  req_logger <- katipAddNamespace (Namespace ["request"]) askLoggerIO
  auth_logger <- katipAddNamespace (Namespace ["auth"]) askLoggerIO

  let settings =
        Warp.defaultSettings
          & Warp.setPort cfgServerPort
          & Warp.setOnException (logUncaughtException excep)
          & Warp.setOnExceptionResponse (\e -> mk500Response e (coerce cfgServerError) mute500)
          & Warp.setServerName ("scaffold api server, revision " <> $gitCommit)
          & Warp.setLogger (logRequest req_logger)
  let multipartOpts =
        (defaultMultipartOptions (Proxy @Tmp))
          { generalOptions = clearMaxRequestNumFiles defaultParseRequestBodyOptions
          }
  let mkCtx = formatters :. defaultJWTSettings (configKatipEnv ^. jwk) :. defaultCookieSettings :. EmptyContext
  let runServer = serveWithContext (withSwagger api) mkCtx server

  mware_logger <- katipAddNamespace (Namespace ["middleware"]) askLoggerWithLocIO
  serverAsync <- liftIO $ async $ Warp.runSettings settings $ do 
    let toIO = runKatipContextT logEnv () ns
    middleware cfgCors mware_logger $ Katip.Wai.runApplication toIO $ mkApplication runServer    
  
  telnyx_logger <- katipAddNamespace (Namespace ["telnyx"]) askLoggerIO
  let telnyxEnv = Job.Telnyx.TelnyxEnv { logger = telnyx_logger, pool = katipEnvHasqlDbPool configKatipEnv }
  telnyx <- liftIO $ async $ Job.Telnyx.makeCall telnyxEnv

  liftIO (void (waitAnyCancel [serverAsync, telnyx])) `logExceptionM` ErrorS

middleware :: Cfg.Cors -> KatipLoggerLocIO -> Application -> Application
middleware cors log app = mkCors cors app

logUncaughtException :: KatipLoggerIO -> Maybe Request -> SomeException -> IO ()
logUncaughtException log req e =
  when (Warp.defaultShouldDisplayException e) $
    maybe
      ( log ErrorS (logStr ("before request being handled" <> show e)))
      ( \r -> log ErrorS (logStr ("\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e)))
      req

mk500Response :: SomeException -> Bool -> Maybe Bool -> Response
mk500Response error cfgServerError mute500 =
  bool
    ( responseLBS
        status200
        [ (H.hContentType, "application/json; charset=utf-8"),
          (hAccessControlAllowOrigin, "*")
        ]
        $ encode @(Response.Response ())
        $ Response.Error (asError @T.Text (showt error))
    )
    mk500
    cfgServerError
  where
    mk500 =
      case mute500 of
        Just True ->
          responseLBS
            status500
            [ (H.hContentType, "text/plain; charset=utf-8"),
              (hAccessControlAllowOrigin, "*")
            ]
            (showt error ^. textbsl)
        _ ->
          responseLBS
            status200
            [ (H.hContentType, "text/json; charset=utf-8"),
              (hAccessControlAllowOrigin, "*")
            ]
            ( encode @(Response.Response ()) $
                Response.Error (asError @T.Text (showt error))
            )

logRequest :: KatipLoggerIO -> Request -> Status -> Maybe Integer -> IO ()
logRequest log req _ _ = log InfoS (logStr (show req))

deriving instance Generic CorsResourcePolicy

mkCors :: Cfg.Cors -> Middleware
mkCors cfg_cors =
  cors $
    const $
      pure $
        simpleCorsResourcePolicy
          & field @"corsOrigins"
            .~ fmap ((,True) . map toS) (Cfg.corsOrigins cfg_cors)
          & field @"corsRequestHeaders"
            .~ [hAuthorization, hContentType, hOrigin]
          & field @"corsMethods"
            .~ simpleMethods
              <> [methodPut, methodPatch, methodDelete, methodOptions]
          & field @"corsIgnoreFailures" .~ True

askLoggerWithLocIO :: KatipContextT AppM (Maybe Loc -> Severity -> LogStr -> IO ())
askLoggerWithLocIO = do
  ctx <- getKatipContext
  ns <- getKatipNamespace
  logEnv <- getLogEnv
  pure $ \loc sev msg ->
    runKatipT logEnv $
      logItem ctx ns loc sev msg

mkApplication :: Application -> Katip.Wai.ApplicationT (KatipContextT IO)
mkApplication hoistedApp = Katip.Wai.middleware DebugS $ \request send ->
  withRunInIO $ \toIO -> hoistedApp request (toIO . send)