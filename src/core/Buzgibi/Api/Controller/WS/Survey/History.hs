{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Buzgibi.Api.Controller.WS.Survey.History (controller, Report, Resource (..), Voice) where

import Buzgibi.Api.Controller.User.GetHistory (mkStatus)
import qualified Buzgibi.Statement.User.Survey as Survey
import Buzgibi.Api.Controller.WS.Utils
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, ToJSON, Value (String))
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson.WithField
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import TH.Mk
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Swagger.Schema.Extended (modify)
import Data.Proxy (Proxy (..))

data Resource = ResourceVoice | ResourceReport deriving (Show)

mkEnumConvertor ''Resource
mkParamSchemaEnum ''Resource [|isoResource . to (modify (Proxy @Resource)) . stext . to String|]
mkFromHttpApiDataEnum ''Resource [|from stext . from isoResource . to Right|]

data Report = Report { reportSurvey :: Int64, reportReport :: Int64 }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Report)]]
          Report

newtype Page = Page Int64
  deriving newtype (FromJSON, Show)

data Voice = Voice { voiceSurvey :: Int64, voiceVoice :: Int64 }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Voice)]]
          Voice

type instance Listen "voice" Voice = ()
type instance Listen "report" (Maybe (WithField "status" Survey.Status Report)) = ()

controller :: AuthenticatedUser -> WS.Connection -> Resource -> KatipControllerM ()
controller user conn ResourceReport = withWS @Int conn $ \db _ -> liftIO $ listen @"report" @(Maybe (WithField "status" Survey.Status Report)) conn db (coerce user) $ fmap (first mkStatus)
controller user conn ResourceVoice = withWS @Page conn $ \db _ -> liftIO $ listen @"voice" @Voice conn db (coerce user) id