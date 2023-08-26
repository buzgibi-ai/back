{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.User (AuthApi (..), UserApi (..)) where

import Buzgibi.Api.Controller.User.Survey.Make (Survey)
import Buzgibi.Api.Controller.User.GetHistory (History)
import Buzgibi.Api.Controller.User.Survey.Edit (EditSurvey)
import Buzgibi.Api.Controller.User.Survey.Submit (SubmitSurvey)
import Buzgibi.Statement.User.Notification (GetNotification)
import Buzgibi.Api.Controller.Auth.Password.MakeLink (ResetPasswordLink)
import Buzgibi.Api.Controller.Auth.Password.Create (NewPassword)
import Buzgibi.Auth (AuthenticatedUser, JWT)
import Buzgibi.Transport.Model.User
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import Data.Int (Int64)
import Data.Text (Text)

data AuthApi route = AuthApi
  { _authApiLogin ::
      route
        :- "login"
          :> Capture "auth_type" AuthType
          :> ReqBody '[JSON] Credentials
          :> Post '[JSON] (Response AuthToken),
    _authApiRegister ::
      route
        :- "register"
          :> ReqBody '[JSON] Credentials
          :> Post '[JSON] (Response AuthToken),
    _authApiLogout ::
      route
        :- "logout"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Post '[JSON] (Response ()),
    _authApiEmailConfirm ::
      route
        :- "email"
          :> "confirm"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> QueryParam "key" Text
          :> Get '[JSON] (Response Bool),
    _authApiEmailLinkSend ::
      route
        :- "email"
          :> "link"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Put '[JSON] (Response (Maybe Int)),
    _authApiResetPassMakeLink ::
      route
        :- "password"
          :> "reset"
          :> "link"
          :> ReqBody '[JSON] ResetPasswordLink
          :> Put '[JSON] (Response (Maybe Int64)),
    _authApiResetPassNewPass ::
      route
        :- "password"
          :> "reset"
          :> ReqBody '[JSON] NewPassword
          :> Post '[JSON] (Response Bool) 
  }
  deriving stock (Generic)

data UserApi route = UserApi
  { _userApiGetProfile ::
      route
        :- "profile"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ()),
    _userApiMakeSurvey ::
      route
        :- "survey"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] Survey
          :> Put '[JSON] (Response ()),
    _userApiEditSurvey ::
      route
        :- "survey"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Capture "survey" Int64
          :> ReqBody '[JSON] EditSurvey
          :> Post '[JSON] (Response Bool),
    _userApiSubmitSurvey ::
      route
        :- "survey"
          :> "submit"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] SubmitSurvey
          :> Post '[JSON] (Response Bool),          
    _userApiGetEnquiryHistory ::
      route
        :- "survey"
          :> "history"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> QueryParam' '[Optional, Strict] "page" Int
          :> Get '[JSON] (Response History),
    _userApiNotificationsGet ::
      route
        :- "notifications"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response [GetNotification])
  }
  deriving stock (Generic)
