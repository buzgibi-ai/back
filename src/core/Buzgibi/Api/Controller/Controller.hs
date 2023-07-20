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
import qualified Buzgibi.Api.Controller.User.GetHistory as User.GetHistory
import qualified Buzgibi.Api.Controller.User.GetProfile as User.GetProfile
import qualified Buzgibi.Api.Controller.User.MakeSurvey  as User.MakeSurvey
import qualified Buzgibi.Api.Controller.Webhook.CatchBark as Webhook.CatchBark
import qualified Buzgibi.Api.Controller.Webhook.CatchTelnyx as Webhook.CatchTelnyx
import qualified Buzgibi.Auth as Auth
import Katip
import Katip.Controller
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
      _httpApiUser = toServant user,
      _httpApiForeign = toServant _foreign,
      _httpApiReCaptcha = toServant captcha
    }

file :: FileApi (AsServerT KatipControllerM)
file =
  FileApi
    { _fileApiUpload = \auth bucket files ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "upload"])
              (File.Upload.controller user bucket files),
      _fileApiPatch = \auth fid file ->
        auth `Auth.withAuth` \_ ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "patch"])
              (File.Patch.controller fid file),
      _fileApiDelete = \auth ident ->
        auth `Auth.withAuth` \_ ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "delete"])
              (File.Delete.controller ident),
      _fileApiDownload = \userId option fid w h ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["file", "download"])
            (File.Download.controller userId option fid w h)
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
      _authApiLogout = \auth ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "logout"])
              (Auth.Logout.controller user)
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
        \token ->
          flip
            logExceptionM
            ErrorS
            $ katipAddNamespace
              (Namespace ["frontend", "init"])
              (Frontend.Init.controller token),
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

user :: UserApi (AsServerT KatipControllerM)
user =
  UserApi
    { _userApiGetProfile = \auth ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "profile", "get"])
            $ User.GetProfile.controller ident,
      _userApiMakeEnquiry = \auth enquiry ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey ", "make"])
            $ User.MakeSurvey.controller ident enquiry,
      _userApiGetEnquiryHistory = \auth page ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey", "history"])
            $ User.GetHistory.controller ident page
    }

_foreign :: ForeignApi (AsServerT KatipControllerM)
_foreign =
  ForeignApi
    { _foreignApiSendGrid = toServant sendgrid,
      _foreignApiWebhook = toServant webhook
    }

sendgrid :: SendGridApi (AsServerT KatipControllerM)
sendgrid =
  SendGridApi
    { _sendGridApiSendMail =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "send"])
          . SendGrid.Send.controller
    }

webhook :: WebhookApi (AsServerT KatipControllerM)
webhook =
  WebhookApi
    { _webhookApiBark =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["webhook", "bark"])
          . Webhook.CatchBark.controller
    , _webhookApiTelnyx = 
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["webhook", "telnyx"])
          . Webhook.CatchTelnyx.controller
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
