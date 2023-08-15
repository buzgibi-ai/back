{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Buzgibi.Api.Controller.WS.Survey.History (controller) where

import Buzgibi.Api.Controller.WS.Utils
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)

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

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller _ conn = withWS @Page conn $ \db _ -> liftIO $ listen @"voice" @Voice conn db id