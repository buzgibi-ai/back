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

module Buzgibi.Api.Controller.Controller (controller) where

-- controllers

import Control.Monad.Except
import Katip
import KatipController
import Buzgibi.Api
import qualified Buzgibi.Api.Controller.File.Delete as File.Delete
import qualified Buzgibi.Api.Controller.File.Download as File.Download
import qualified Buzgibi.Api.Controller.File.Patch as File.Patch
import qualified Buzgibi.Api.Controller.File.Upload as File.Upload
import qualified Buzgibi.Api.Controller.Frontend.GetCookies as Frontend.GetCookies
import qualified Buzgibi.Api.Controller.Frontend.GetMeta as Frontend.GetMeta
import qualified Buzgibi.Api.Controller.Frontend.Init as Frontend.Init
import qualified Buzgibi.Api.Controller.Frontend.Log as Frontend.Log
import qualified Buzgibi.Api.Controller.Frontend.Translate as Frontend.Translate
import qualified Buzgibi.Api.Controller.ReCaptcha.Verify as ReCaptcha.Verify
import qualified Buzgibi.Api.Controller.SendGrid.SendMail as SendGrid.Send
import qualified Buzgibi.Auth as Auth
import Buzgibi.Transport.Model.User (BasicAuth (..))
import Buzgibi.Transport.Response
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

user :: Auth.AuthenticatedUser -> UserApi (AsServerT KatipControllerM)
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
