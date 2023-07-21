{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Buzgibi.Transport.Model.Telnyx 
       (AppRequest (..), 
        AppResponse (..),
        CallRequest (..),
        CallResponse (..),
        CallResponseData,
       ) where

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

-- https://developers.telnyx.com/openapi/callcontrol/tag/Call-Commands/#tag/Call-Commands/operation/callDial
data CallRequest = 
     CallRequest
     {
      -- The DID or SIP URI to dial out to. Multiple DID or SIP URIs can be provided using an array of strings
      callRequestTo :: ![T.Text],
      -- The from number to be used as the caller id presented to the destination (to number). The number should be in +E164 format.
      callRequestFrom :: T.Text,
      -- The from_display_name string to be used as the caller id name (SIP From Display Name) presented to the destination (to number). 
      -- The string should have a maximum of 128 characters, containing only letters, numbers, spaces, and -_~!.+ special characters. 
      -- If ommitted, the display name will be the same as the number in the from field.
      callRequestFromDisplayName :: !T.Text,
      -- The ID of the Call Control App (formerly ID of the connection) to be used when dialing the destination
      callRequestConnectionId :: !T.Text,
      -- The URL of a file to be played back to the callee when the call is answered. 
      -- The URL can point to either a WAV or MP3 file. media_name and audio_url cannot be used together in one request.
      callRequestAudioUrl :: !T.Text
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor CallRequest)]]
          CallRequest

newtype CallResponseData = CallResponseData { callResponse :: CallResponse }

instance FromJSON CallResponseData where
  parseJSON = withObject "CallResponseData" $ \o -> do
    _data <- o .: "data"
    fmap CallResponseData $ parseJSON @CallResponse _data

data CallResponse =
     CallResponse
     {
      callResponseRecordType :: !T.Text,
      -- ID that is unique to the call session and can be used to correlate webhook events. 
      -- Call session is a group of related call legs that 
      -- logically belong to the same phone call, e.g. an inbound and outbound leg of a transferred call
      callResponseCallSessionId :: !T.Text,
      -- ID that is unique to the call and can be used to correlate webhook events
      callResponseCallLegId :: !T.Text,
      -- Unique identifier and token for controlling the call.
      callResponseCallControlId :: !T.Text,
      -- Indicates whether the call is alive or not. For Dial command it will always be false (dialing is asynchronous).
      callResponseIsAlive :: !Bool
     } 
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor CallResponse)]]
          CallResponse