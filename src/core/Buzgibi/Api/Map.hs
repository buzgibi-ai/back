{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Map
  ( HttpApi (..),
    module File,
    module User,
    module Front,
    module Foreign,
    module ReCaptcha,
  )
where

import Buzgibi.Api.File as File
import Buzgibi.Api.Foreign as Foreign
import Buzgibi.Api.Frontend as Front
import Buzgibi.Api.ReCaptcha as ReCaptcha
import Buzgibi.Api.User as User
import Buzgibi.Auth
import Servant.API
import Servant.API.Generic
import qualified Servant.Auth.Server as SA
import Servant.Swagger.Tags

data HttpApi route = HttpApi
  { _httpApiFile ::
      route
        :- Tags "File"
          :> "file"
          :> ToServant FileApi AsApi,
    _httpApiAuth ::
      route
        :- Tags "Auth"
          :> "auth"
          :> ToServant AuthApi AsApi,
    _httpApiFront ::
      route
        :- Tags "Front"
          :> "frontend"
          :> ToServant FrontendApi AsApi,
    _httpApiUser ::
      route
        :- Tags "User"
          :> "user"
          :> SA.Auth '[SA.BasicAuth, SA.JWT] User
          :> ToServant UserApi AsApi,
    _httpApiForeign ::
      route
        :- Tags "Foreign"
          :> "foreign"
          :> ToServant ForeignApi AsApi,
    _httpApiReCaptcha ::
      route
        :- Tags "ReCaptcha"
          :> "captcha"
          :> ToServant ReCaptchaApi AsApi
  }
  deriving stock (Generic)
