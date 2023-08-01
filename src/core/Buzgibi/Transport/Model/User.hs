{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Buzgibi.Transport.Model.User (AuthToken (..), Credentials (..), AuthType) where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import GHC.Exts
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (parseQueryParam))
import TH.Mk

data AuthType = JWT
  deriving stock (Generic)
  deriving (Enum)

mkToSchemaAndJSON ''AuthType
mkEnumConvertor ''AuthType
mkParamSchemaEnum ''AuthType [|isoAuthType . jsonb|]
mkFromHttpApiDataEnum ''AuthType [|from stext . from isoAuthType . to Right|]

newtype AuthToken = AuthToken Text
  deriving stock (Generic, Show)
  deriving anyclass (ToParamSchema)
  deriving newtype (ToJSON, FromJSON)

instance FromHttpApiData AuthToken where
  parseQueryParam = Right . AuthToken

instance ToSchema AuthToken where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AuthToken") $
        toSchema (Proxy @Text)

data Credentials = Credentials {email :: Text, password :: Text}
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor Credentials)]]
          Credentials

instance ToSchema Credentials where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "Credentials") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ fromList [("email", textSchema), ("password", textSchema)]
