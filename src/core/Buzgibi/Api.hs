{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api
  ( Api (..),
    HttpApi (..),
    FileApi (..),
    AuthApi (..),
    FrontendApi (..),
    UserApi (..),
    SendGridApi (..),
    ForeignApi (..),
    ReCaptchaApi (..),
    WebhookApi (..),
    WSApi (..),
    api,
    swaggerHttpApi,
  )
where

import BuildInfo
import Buzgibi.Api.Map
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Proxy
import Data.Swagger
import Servant.API
import Servant.API.Generic
import Servant.Auth.Swagger ()
import Servant.Swagger
import Servant.Swagger.RawM ()

newtype Api route = Api {_apiHttp :: route :- ToServant HttpWrapperApi AsApi} deriving stock (Generic)

newtype HttpWrapperApi route = HttpWrapperApi
  { _httpWrapperApiApi ::
      route
        :- Description "Buzgibi api"
          :> ToServant HttpApi AsApi
  }
  deriving stock (Generic)

api :: Proxy (ToServantApi Api)
api = genericApi (Proxy :: Proxy Api)

swaggerHttpApi :: String -> Maybe Int -> Version -> Swagger
swaggerHttpApi url port ver =
  toSwagger (genericApi (Proxy @HttpWrapperApi))
    & schemes ?~ [Http, Https]
    & host ?~ Host url (fmap fromIntegral port)
    & info . description ?~ "Buzgibi server api" ^. stext
    & info . version .~ show ver ^. stext
    & info . contact ?~ Contact Nothing Nothing (Just ("fclaw007@gmail.com" ^. stext))
    & info . title .~ "Buzgibi. Tag (" <> $gitTag <> "). Commit (" <> $gitCommit <> ")"
