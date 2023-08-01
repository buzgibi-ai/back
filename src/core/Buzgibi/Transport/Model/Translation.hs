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
{-# LANGUAGE DeriveAnyClass #-}

module Buzgibi.Transport.Model.Translation where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Default.Class
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep, typeRepTyCon)
import GHC.Exts
import GHC.Generics (Generic)
import TH.Mk
import Control.DeepSeq (NFData)
import Data.Text.Extended ()

data Page = PageHome | PageAuth
  deriving stock (Generic, Show, Eq, Ord)
  deriving (Enum)

instance NFData Page

instance Default Page where
  def = PageHome

data Menu = MenuHome | MenuSignUp | MenuSignIn | MenuHistory | MenuMakeSurvey
  deriving stock (Generic, Show, Eq, Ord)
  deriving (Enum)

instance NFData Menu

instance Default Menu where
  def = MenuSignUp

data Resource = ResourceUser | ResourceStub
  deriving stock (Generic, Show, Eq, Ord)
  deriving (Enum)

instance NFData Resource

instance Default Resource where
  def = ResourceUser

data Endpoints = EndpointsMakeSurvey | EndpointsHistory
  deriving stock (Generic, Show, Eq, Ord)
  deriving (Enum)

instance NFData Endpoints

instance Default Endpoints where
  def = EndpointsMakeSurvey

mkToSchemaAndJSON ''Page
mkEnumConvertor ''Page

mkToSchemaAndJSON ''Menu
mkEnumConvertor ''Menu

mkToSchemaAndJSON ''Resource
mkEnumConvertor ''Resource

mkToSchemaAndJSON ''Endpoints
mkEnumConvertor ''Endpoints

data Lang = English | Turkish
  deriving stock (Generic, Eq, Ord)
  deriving (Enum)

instance NFData Lang

instance Show Lang where
  show English = "english"
  show Turkish = "turkish"

instance Default Lang where
  def = English

data Map k v = Map {mapKey :: !k, mapValue :: !v}
  deriving stock (Generic, Show, Eq, Ord)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructorParamType (Map k v))]]
          (Map k v)
  deriving anyclass Default        

instance (NFData k, NFData v) => NFData (Map k v)

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
  { translationPage :: ![Map Page [Map T.Text T.Text]],
    translationMenu :: ![Map Menu T.Text],
    translationCopyright :: !T.Text,
    translationResource :: ![Map Resource [Map T.Text T.Text]],
    translationEndpoints :: ![Map Endpoints [Map T.Text T.Text]]
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Translation)]]
          Translation
  deriving anyclass Default        

instance NFData Translation

deriveToSchemaFieldLabelModifier ''Translation [|modify (Proxy @Translation)|]

mkToSchemaAndJSON ''Lang
mkEnumConvertor ''Lang
mkParamSchemaEnum ''Lang [|isoLang . jsonb|]
mkFromHttpApiDataEnum ''Lang [|from stext . from isoLang . to Right|]