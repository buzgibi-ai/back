{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.Frontend.GetMeta (controller, Meta) where

import Buzgibi.Transport.Response
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import GHC.Generics hiding (Meta)
import Katip.Controller

data Meta = Meta {description :: !T.Text, robot :: !(Maybe T.Text)}
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor Meta)]]
          Meta

deriveToSchemaFieldLabelModifier
  ''Meta
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @Meta))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: Maybe T.Text -> KatipControllerM (Response Meta)
controller _ = undefined
