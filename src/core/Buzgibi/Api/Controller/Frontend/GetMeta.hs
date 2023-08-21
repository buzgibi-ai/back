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
{-# LANGUAGE InstanceSigs #-}

module Buzgibi.Api.Controller.Frontend.GetMeta (controller, Meta, Page) where

import Buzgibi.Transport.Response
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import GHC.Generics (Generic)
import Katip.Controller
import TH.Mk
import Control.Lens
import Control.Lens.Iso.Extended (stext)
import System.Directory (getCurrentDirectory)
import qualified Data.Text.IO as T (readFile)
import Control.Monad.IO.Class (liftIO)

data Meta = Meta {description :: !T.Text, robot :: !(Maybe T.Text)}
  deriving stock (Generic, Show)
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

data Page = Home

mkEnumConvertor ''Page
mkParamSchemaEnum ''Page [|isoPage . stext . to String|]
mkFromHttpApiDataEnum ''Page [|from stext . from isoPage . to Right|]

controller :: Maybe Page -> KatipControllerM (Response Meta)
controller Nothing = return $ Ok $ Meta mempty Nothing
controller (Just page) = do 
  description <- liftIO $ do
   dir <- getCurrentDirectory
   T.readFile $ dir <> "/" <> "meta" <> "/" <> page^.isoPage <> ".txt"
  return $ Ok $ Meta description Nothing