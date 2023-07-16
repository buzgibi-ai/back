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
import Buzgibi.Api.Controller.Utils (withError)
import Katip.Controller
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
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
import Data.Int (Int32)

data HistoryItem = 
     HistoryItem
     { 
        historyItemIdent :: !Int64,
        historyItemName :: !T.Text,
        historyItemTimestamp :: !UTCTime
     }
     deriving stock (Generic)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor HistoryItem)]]
          HistoryItem

deriveToSchemaFieldLabelModifier ''HistoryItem [|modify (Proxy @HistoryItem)|]

data History = 
     History 
     { 
        historyTotal :: !Int32,
        historyPerPage :: !Int32,
        historyItems :: ![HistoryItem]
     }
     deriving stock (Generic)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor History)]]
          History

deriveToSchemaFieldLabelModifier ''History [|modify (Proxy @History)|]

controller :: AuthenticatedUser -> Maybe Int -> KatipControllerM (Response History)
controller user page = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let offset = maybe 1 fromIntegral page
  res <- fmap mkHistory $ transactionM hasql $ statement Enquiry.getHistory (coerce user, offset)
  return $ withError res id

mkHistory (Just (xs, total)) = let xs' = sequence (map (eitherDecode . encode) xs) in fmap (History total 10) xs'
mkHistory _ = Right $ History 0 0 []