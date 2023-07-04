{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Buzgibi.Transport.Model.User (AuthToken (..), Credentials (..), AuthType) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import GHC.Exts
import GHC.Generics (Generic)
import TH.Mk
import Control.Lens.Iso.Extended (jsonb, stext)

data AuthType = Basic | JWT
  deriving stock (Generic)
  deriving (Enum)

mkToSchemaAndJSON ''AuthType
mkEnumConvertor ''AuthType
mkParamSchemaEnum ''AuthType [|isoAuthType . jsonb|]
mkFromHttpApiDataEnum ''AuthType [|from stext . from isoAuthType . to Right|]

newtype AuthToken = AuthToken Text
  deriving stock (Generic)
  deriving anyclass (ToParamSchema)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema AuthToken where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AuthToken") $
        toSchema (Proxy @Text)

data Credentials = Credentials {login :: Text, password :: Text}
  deriving stock (Generic)
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
          & properties .~ fromList [("login", textSchema), ("password", textSchema)]