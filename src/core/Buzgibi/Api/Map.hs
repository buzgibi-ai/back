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
    module WS
  )
where

import Buzgibi.Api.File as File
import Buzgibi.Api.Foreign as Foreign
import Buzgibi.Api.Frontend as Front
import Buzgibi.Api.ReCaptcha as ReCaptcha
import Buzgibi.Api.User as User
import Buzgibi.Api.WS as WS
import Servant.API
import Servant.API.Generic
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
          :> ToServant ReCaptchaApi AsApi,
    _httpApiWS ::
      route
        :- Tags "WS"
          :> "ws"
          :> ToServant WSApi AsApi
  }
  deriving stock (Generic)
