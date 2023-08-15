{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Buzgibi.Api.Controller.WS.Survey.Report (controller, Report) where

import Buzgibi.Api.Controller.User.GetHistory (mkStatus)
import qualified Buzgibi.Statement.User.Survey as Survey
import Buzgibi.Api.Controller.WS.Utils
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson.WithField
import Data.Bifunctor (first)
import Data.Coerce (coerce)

data Report = Report { reportSurvey :: Int64, reportReport :: Int64 }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Report)]]
          Report

type instance Listen "report" (Maybe (WithField "status" Survey.Status Report)) = ()

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller user conn = 
  withWS @Int conn $ \db _ -> 
    liftIO $ listen @"report" @(Maybe (WithField "status" Survey.Status Report)) conn db (coerce user) $ fmap (first mkStatus)