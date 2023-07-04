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
import Servant.API.Extended (JSON, Post, ReqBody, type (:>), Capture)
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype AuthApi route = AuthApi
  { _authApiAuthWithBasic ::
      route
        :- "auth"
          :> "login"
          :> Capture "auth_type" AuthType
          :> ReqBody '[JSON] Credentials
          :> Post '[JSON] (Response AuthToken)
  }
  deriving stock (Generic)

newtype UserApi route = UserApi
  { _userApiGetProfile ::
      route
        :- "profile"
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
