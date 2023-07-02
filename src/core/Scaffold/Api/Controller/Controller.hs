{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Scaffold.Api.Controller.Controller (controller) where

-- controllers

import Control.Monad.Except
import Katip
import KatipController
import Scaffold.Api
import qualified Scaffold.Api.Controller.File.Delete as File.Delete
import qualified Scaffold.Api.Controller.File.Download as File.Download
import qualified Scaffold.Api.Controller.File.Patch as File.Patch
import qualified Scaffold.Api.Controller.File.Upload as File.Upload
import qualified Scaffold.Api.Controller.Frontend.GetCookies as Frontend.GetCookies
import qualified Scaffold.Api.Controller.Frontend.GetMeta as Frontend.GetMeta
import qualified Scaffold.Api.Controller.Frontend.Init as Frontend.Init
import qualified Scaffold.Api.Controller.Frontend.Log as Frontend.Log
import qualified Scaffold.Api.Controller.Frontend.Translate as Frontend.Translate
import qualified Scaffold.Api.Controller.ReCaptcha.Verify as ReCaptcha.Verify
import qualified Scaffold.Api.Controller.SendGrid.SendMail as SendGrid.Send
import Scaffold.Auth
import Scaffold.Transport.Model.User (BasicAuth (..))
import Scaffold.Transport.Response
import Servant.API.Generic
import Servant.Auth.Server (AuthResult (..), wwwAuthenticatedErr)
import Servant.RawM.Server ()
import Servant.Server.Generic

controller :: Api (AsServerT KatipControllerM)
controller = Api {_apiHttp = toServant httpApi}

httpApi :: HttpApi (AsServerT KatipControllerM)
httpApi =
  HttpApi
    { _httpApiFile = toServant file,
      _httpApiAuth = toServant auth,
      _httpApiFront = toServant frontend,
      _httpApiUser =
        \case
          Authenticated u -> toServant $ user u
          _ ->
            const $
              throwError $
                wwwAuthenticatedErr
                  "only for authorized personnel",
      _httpApiForeign = toServant _foreign,
      _httpApiReCaptcha = toServant captcha
    }

file :: FileApi (AsServerT KatipControllerM)
file =
  FileApi
    { _fileApiUpload = \bucket files ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["file", "upload"])
            (File.Upload.controller bucket files),
      _fileApiPatch = \fid file ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["file", "patch"])
            (File.Patch.controller fid file),
      _fileApiDelete =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["file", "delete"])
          . File.Delete.controller,
      _fileApiDownload = \option fid w h ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["file", "download"])
            (File.Download.controller option fid w h)
    }

auth :: AuthApi (AsServerT KatipControllerM)
auth =
  AuthApi
    { _authApiAuthWithBasic = \_ ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["auth", "login", "basic"])
            (return $ Ok $ BasicAuth "ZmNsYXcwMDdAZ21haWwuY29tOnRlc3Q=")
    }

frontend :: FrontendApi (AsServerT KatipControllerM)
frontend =
  FrontendApi
    { _frontendApiLog = \req ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["frontend", "log"])
            (Frontend.Log.controller req),
      _frontendApiInit =
        flip
          logExceptionM
          ErrorS
          ( katipAddNamespace
              (Namespace ["frontend", "init"])
              Frontend.Init.controller
          ),
      _frontendApiTranslate =
        \lang ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["frontend", "translate"])
              (Frontend.Translate.controller lang),
      _frontendApiGetCookies =
        flip
          logExceptionM
          ErrorS
          ( katipAddNamespace
              (Namespace ["frontend", "cookies"])
              Frontend.GetCookies.controller
          ),
      _frontendApiGetMeta =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["frontend", "meta"])
          . Frontend.GetMeta.controller
    }

user :: User -> UserApi (AsServerT KatipControllerM)
user _ =
  UserApi
    { _userApiGetProfile = \_ ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["user", "profile", "get"])
            undefined
    }

_foreign :: ForeignApi (AsServerT KatipControllerM)
_foreign = ForeignApi {_foreignApiSendGrid = toServant sendgrid}

sendgrid :: SendGridApi (AsServerT KatipControllerM)
sendgrid =
  SendGridApi
    { _sendGridApiSendMail =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "send"])
          . SendGrid.Send.controller
    }

captcha :: ReCaptchaApi (AsServerT KatipControllerM)
captcha =
  ReCaptchaApi
    { _reCaptchaApiVerify =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["captcha", "validate"])
          . ReCaptcha.Verify.controller
    }
