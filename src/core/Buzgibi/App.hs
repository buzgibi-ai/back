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
import Buzgibi.Job.OpenAI as Job.OpenAI
import Buzgibi.Job.Survey as Job.Survey
import Buzgibi.Job.Report as Job.Report
import Buzgibi.Api
import Buzgibi.EnvKeys (Telnyx (..), OpenAI (..), Sendgrid)
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
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header.Extended
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
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
import Data.Maybe (fromMaybe)
import qualified Network.Minio as Minio
import Control.Concurrent.MVar.Lifted
import qualified Control.Monad.State.Class as S
import Data.Tuple (swap)
import Data.Bifunctor (first)
import Data.Time.Clock (getCurrentTime)

data Cfg = Cfg
  { cfgHost :: !String,
    cfgSwaggerPort :: !(Maybe Int),
    cfgServerPort :: !Int,
    cfgCors :: !Cfg.Cors,
    cfgServerError :: !Cfg.ServerError,
    mute500 :: !(Maybe Bool),
    ns :: !Namespace,
    logEnv :: !LogEnv,
    telnyxCfg :: !(Maybe Telnyx),
    openaiCfg :: !(Maybe OpenAI),
    manager :: !HTTP.Manager,
    minio :: !(Minio.MinioConn, T.Text),
    webhook :: !T.Text,
    jobFrequency :: !Int,
    sendgridCfg :: !(Maybe Sendgrid)
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

  stateRef <- fmap getKatipState S.get >>= newMVar

  let hoistedServer =
        hoistServerWithContext
          (withSwagger api)
          (Proxy @'[CookieSettings, JWTSettings])
          (\controller -> do
              modifyMVar stateRef $ \old -> do
                fmap ((first getState) . swap) $ 
                  runKatipController cfg (State old) $ do 
                    resp <- controller
                    S.modify' $ \(State new) -> 
                      State $ if old /= new then new else old
                    return resp)
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
          & Warp.setServerName ("buzgibi api server, revision " <> $gitCommit)
          & Warp.setLogger (logRequest req_logger)
  let multipartOpts =
        (defaultMultipartOptions (Proxy @Tmp))
          { generalOptions = 
              clearMaxRequestNumFiles $ 
                setMaxRequestKeyLength 100 
                  defaultParseRequestBodyOptions
          }
  let mkCtx = multipartOpts :. formatters :. defaultJWTSettings (configKatipEnv ^. jwk) :. defaultCookieSettings :. EmptyContext

  mware_logger <- katipAddNamespace (Namespace ["middleware"]) askLoggerWithLocIO
  serverAsync <- liftIO $ async $ Warp.runSettings settings $ do 
    let toIO = runKatipContextT logEnv () ns
    middleware cfgCors mware_logger $ 
      Katip.Wai.runApplication toIO $ 
        mkApplication $ serveWithContext (withSwagger api) mkCtx hoistedServer
  
  telnyx_logger <- katipAddNamespace (Namespace ["job", "telnyx"]) askLoggerIO
  let telnyxEnv =
        Job.Telnyx.TelnyxCfg
        { logger = telnyx_logger,
          pool = katipEnvHasqlDbPool configKatipEnv, 
          telnyxCfg = fromMaybe (error "telnyx not set") telnyxCfg,
          manager = manager,
          webhook = webhook,
          jobFrequency = jobFrequency
        }
  telnyxApp <- liftIO $ async $ Job.Telnyx.makeApp telnyxEnv
  telnyxCall <- liftIO $ async $ Job.Telnyx.makeCall telnyxEnv

  openai_logger <- katipAddNamespace (Namespace ["job", "openai"]) askLoggerIO
  let openAICfg =
        Job.OpenAI.OpenAICfg
        { logger = openai_logger,
          pool = katipEnvHasqlDbPool configKatipEnv, 
          openaiCfg = fromMaybe (error "openai not set") openaiCfg,
          manager = manager,
          minio = fst minio,
          jobFrequency = jobFrequency
        }
  openaiTranscrip <- liftIO $ async $ Job.OpenAI.getTranscription openAICfg
  openaiSA <- liftIO $ async $ Job.OpenAI.performSentimentalAnalysis openAICfg

  survey_logger <- katipAddNamespace (Namespace ["job", "survey"]) askLoggerIO
  let surveyCfg =
        Job.Survey.SurveyCfg
        { logger = survey_logger,
          pool = katipEnvHasqlDbPool configKatipEnv,
          minio = minio,
          jobFrequency = jobFrequency
        }
  survey <- liftIO $ async $ Job.Survey.makeReport surveyCfg

  report_logger <- katipAddNamespace (Namespace ["job", "report"]) askLoggerIO
  let reportCfg =
        Job.Report.ReportCfg
        { logger = report_logger,
          pool = katipEnvHasqlDbPool configKatipEnv,
          telnyxCfg = fromMaybe (error "telnyx not set") telnyxCfg,
          manager = manager,
          sendgridCfg = fromMaybe (error "sendgrid not set") sendgridCfg
        }
  report <- liftIO $ async $ getCurrentTime >>= evalStateT (Job.Report.makeDailyReport reportCfg)

  end <- fmap snd $ flip logExceptionM ErrorS $ liftIO $ waitAnyCatchCancel 
    [serverAsync, telnyxApp, telnyxCall, survey, openaiTranscrip, openaiSA, report]
  
  whenLeft end $ \e -> $(logTM) EmergencyS $ logStr $ "server has been terminated. error " <> show e


middleware :: Cfg.Cors -> KatipLoggerLocIO -> Application -> Application
middleware cors log app = mkCors cors app

logUncaughtException :: KatipLoggerIO -> Maybe Request -> SomeException -> IO ()
logUncaughtException log req e =
  when (Warp.defaultShouldDisplayException e) $
    maybe
      ( log CriticalS (logStr ("before request being handled" <> show e)))
      ( \r -> log CriticalS (logStr ("\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e)))
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