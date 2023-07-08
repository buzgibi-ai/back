{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.User (AuthApi (..), UserApi (..)) where

import Buzgibi.Transport.Model.User
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended (Capture, Get, JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import qualified Servant.Auth.Server as SA
import Buzgibi.Auth (AuthenticatedUser, JWT)

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

newtype UserApi route = UserApi
  { _userApiGetProfile ::
      route
        :- "profile"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ())
  }
  deriving stock (Generic)
