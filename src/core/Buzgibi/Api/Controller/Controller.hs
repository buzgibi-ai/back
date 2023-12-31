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
import qualified Buzgibi.Api.Controller.User.Survey.Make as User.Survey.Make
import qualified Buzgibi.Api.Controller.User.Survey.Edit as User.Survey.Edit
import qualified Buzgibi.Api.Controller.User.Survey.Submit as User.Survey.Submit
import qualified Buzgibi.Api.Controller.Webhook.CatchBark as Webhook.CatchBark
import qualified Buzgibi.Api.Controller.Webhook.CatchTelnyx as Webhook.CatchTelnyx
import qualified Buzgibi.Api.Controller.Webhook.CatchGit as Webhook.CatchGit
import qualified Buzgibi.Api.Controller.User.GetNotifications as User.GetNotifications
import qualified Buzgibi.Api.Controller.WS.Survey.History as WS.Survey.History
import qualified Buzgibi.Api.Controller.Auth.Email.Confirm as Auth.Email.Confirm
import qualified Buzgibi.Api.Controller.Auth.Email.SendLink as Auth.Email.SendLink
import qualified Buzgibi.Api.Controller.Auth.Password.MakeLink as Auth.Password.MakeLink
import qualified Buzgibi.Api.Controller.Auth.Password.Create as Auth.Password.Create
import qualified Buzgibi.Auth as Auth
import Katip
import Katip.Controller hiding (webhook)
import Servant.API.Generic
import Servant.RawM.Server ()
import Servant.Server.Generic
import qualified Network.WebSockets.Connection as WS

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
      _httpApiReCaptcha = toServant captcha,
      _httpApiWS = toServant ws
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
              (Auth.Logout.controller user),
      _authApiEmailConfirm = \auth key ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "email", "confirm"])
              (Auth.Email.Confirm.controller user key),
      _authApiEmailLinkSend = \auth ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "email", "link", "send"])
              (Auth.Email.SendLink.controller user),
      _authApiResetPassMakeLink = \email ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "pass", "reset", "link"])
              (Auth.Password.MakeLink.controller email),
      _authApiResetPassNewPass = \pass ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "pass", "reset"])
              (Auth.Password.Create.controller pass)
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
      _userApiMakeSurvey = \auth survey ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey ", "make"])
            $ User.Survey.Make.controller ident survey,
      _userApiGetEnquiryHistory = \auth page ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey", "history"])
            $ User.GetHistory.controller ident page,
      _userApiEditSurvey = \auth surveyIdent edit ->
        auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey", "edit"])
            $ User.Survey.Edit.controller ident surveyIdent edit,
      _userApiSubmitSurvey = \auth submit ->
         auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "survey", "submit"])
            $ User.Survey.Submit.controller ident submit,
     _userApiNotificationsGet = \auth ->
         auth `Auth.withAuth` \ident ->
          flip logExceptionM ErrorS
            $ katipAddNamespace
              (Namespace ["user", "notifications", "get"])
            $ User.GetNotifications.controller ident
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
    , _webhookApiGit = 
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["webhook", "git"])
          . Webhook.CatchGit.controller
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

ws :: WSApi (AsServerT KatipControllerM)
ws = 
  WSApi
  { _wsApiUserHistory = 
      \resource (pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "user", "survey", "history"])
          $ WS.Survey.History.controller ident conn resource      
  }