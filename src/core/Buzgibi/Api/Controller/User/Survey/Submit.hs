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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Buzgibi.Api.Controller.User.Survey.Submit (controller, SubmitWSurvey) where

import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import GHC.Generics (Generic)
import Katip.Controller
import Data.Int (Int64)

data SubmitWSurvey = 
     SubmitWSurvey 
     { submitSurveyIdent :: Int64 
     }
     deriving stock (Generic)
     deriving stock (Show)
     deriving
        (ToJSON, FromJSON)
        via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor SubmitWSurvey)]]
          SubmitWSurvey

deriveToSchemaFieldLabelModifier ''SubmitWSurvey [|modify (Proxy @SubmitWSurvey)|]

controller :: AuthenticatedUser -> SubmitWSurvey -> KatipControllerM (Response ())
controller _ _ = undefined