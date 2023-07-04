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

import Buzgibi.Api
import qualified Buzgibi.Api.Controller.Auth.Login as Auth.Login
import qualified Buzgibi.Api.Controller.Auth.Logout as Auth.Logout
import qualified Buzgibi.Api.Controller.Auth.Register as Auth.Register
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
import Katip
import KatipController
import Servant.API.Generic
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
      _httpApiUser = (`Auth.withAuth` (toServant . user)),
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
    { _authApiLogin = \_ cred ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["auth", "login"])
            (Auth.Login.controller cred),
      _authApiRegister = \cred ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["auth", "register"])
            (Auth.Register.controller cred),
      _authApiLogout =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["auth", "logout"])
          . Auth.Logout.controller
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
    { _userApiGetProfile =
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
