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

module Buzgibi.Api.Controller.User.MakeEnquiry (controller, Enquiry) where

import Buzgibi.Auth (AuthenticatedUser) 
import Buzgibi.Transport.Response
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import KatipController (KatipControllerM)
import qualified Data.Text as T
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))

data Location =
     Location 
     { locationLatitude :: Double, 
       locationLongitude :: Double 
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Location)]]
          Location

data Enquiry = 
     Enquiry 
     { enquiryEnquiry :: !T.Text, 
       enquiryLocation :: Location 
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Enquiry)]]
          Enquiry

deriveToSchemaFieldLabelModifier ''Location [|modify (Proxy @Location)|]
deriveToSchemaFieldLabelModifier ''Enquiry [|modify (Proxy @Enquiry)|]

controller :: AuthenticatedUser -> Enquiry -> KatipControllerM (Response ())
controller _ _ = undefined