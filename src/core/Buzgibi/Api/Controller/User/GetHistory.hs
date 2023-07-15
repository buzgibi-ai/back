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

module Buzgibi.Api.Controller.User.GetHistory (controller, History) where

import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import qualified Buzgibi.Statement.User.Enquiry as Enquiry
import Katip.Controller
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Control.Lens
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))
import Database.Transaction
import Data.Coerce (coerce)

data History = 
     History
     { 
        historyIdent :: !Int64,
        historyName :: !T.Text,
        historyTimestamp :: !UTCTime
     }
     deriving stock (Generic)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor History)]]
          History

deriveToSchemaFieldLabelModifier ''History [|modify (Proxy @History)|]

controller :: AuthenticatedUser -> KatipControllerM (Response [History])
controller user = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap (Ok . map mkHistory) $ transactionM hasql $ statement Enquiry.getHistory $ coerce user

mkHistory (ident, name, time) = History ident name time