{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Buzgibi.Api.Controller.WS.Survey.Report (controller) where

import Buzgibi.Api.Controller.WS.Utils
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)


data Report = Report { reportSurvey :: Int64, reportReport :: Int64 }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Report)]]
          Report

type instance Listen "report" Report = ()

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller _ conn = withWS @() conn $ \db _ -> liftIO $ listen @"report" @Report conn db