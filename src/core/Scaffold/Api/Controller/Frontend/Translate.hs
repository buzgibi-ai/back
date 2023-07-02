{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scaffold.Api.Controller.Frontend.Translate (controller, Lang (..), Translation, Map (..)) where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.Default.Class
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep, typeRepTyCon)
import GHC.Exts
import GHC.Generics (Generic)
import KatipController
import Scaffold.Transport.Response
import TH.Mk

modify :: forall a. Typeable a => String -> String
modify =
  \s ->
    let (head : tail) = show (typeRep (Proxy @a))
     in maybe s (map toLower) (stripPrefix (toLower head : tail) s)

data Lang = English | Turkish
  deriving stock (Generic)
  deriving (Enum)

instance Default Lang where
  def = English

data Map k v = Map {mapKey :: k, mapValue :: v}
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructorParamType (Map k v))]]
          (Map k v)

instance (ToSchema k, ToSchema v, Typeable k, Typeable v) => ToSchema (Map k v) where
  declareNamedSchema _ = do
    key <- declareSchemaRef (Proxy @k)
    value <- declareSchemaRef (Proxy @v)
    let ident =
          T.intercalate
            "."
            [ T.pack (show (typeRepTyCon (typeRep (Proxy @(Map k v))))),
              T.pack (show (typeRep (Proxy @k))),
              T.pack (show (typeRep (Proxy @v)))
            ]
    pure $
      NamedSchema (Just ident) $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ fromList
              [ ("key", key),
                ("value", value)
              ]

data Translation = Translation
  { translationPage :: [Map T.Text T.Text]
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Translation)]]
          Translation

deriveToSchemaFieldLabelModifier ''Translation [|modify @Translation|]

mkToSchemaAndJSON ''Lang
mkEnumConvertor ''Lang
mkParamSchemaEnum ''Lang [|isoLang . jsonb|]
mkFromHttpApiDataEnum ''Lang [|from stext . from isoLang . to Right|]

controller :: Lang -> KatipControllerM (Response Translation)
controller _ = undefined
