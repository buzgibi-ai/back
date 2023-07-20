{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Buzgibi.Transport.Model.Telnyx (AppRequest (..), AppResponse (..)) where


import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia

data AppRequest = 
     AppRequest 
     { appRequestApplicationName :: !T.Text, 
       appRequestWebhookEventUrl :: !T.Text 
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor AppRequest)]]
          AppRequest
-- {
-- the type is a part of the specification detailed in
-- https://developers.telnyx.com/openapi/callcontrol/tag/Call-Control-Applications/#tag/Call-Control-Applications/operation/createCallControlApplication
-- We are interested only in id
-- }
newtype AppResponse = AppResponse { appIdent :: T.Text }

instance FromJSON AppResponse where
  parseJSON = withObject "AppResponse" $ \o -> do
    _data <- o .: "data"
    appIdent <- _data .: "id"
    pure $ AppResponse appIdent