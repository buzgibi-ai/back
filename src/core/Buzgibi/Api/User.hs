{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.User (AuthApi (..), UserApi (..)) where

import Buzgibi.Api.Controller.User.MakeSurvey (Survey)
import Buzgibi.Api.Controller.User.GetHistory (History)
import Buzgibi.Auth (AuthenticatedUser, JWT)
import Buzgibi.Transport.Model.User
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA

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
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)

data UserApi route = UserApi
  { _userApiGetProfile ::
      route
        :- "profile"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ()),
    _userApiMakeEnquiry ::
      route
        :- "survey"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] Survey
          :> Post '[JSON] (Response ()),
    _userApiGetEnquiryHistory ::
      route
        :- "survey"
          :> "history"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> QueryParam' '[Optional, Strict] "page" Int
          :> Get '[JSON] (Response History)
  }
  deriving stock (Generic)
