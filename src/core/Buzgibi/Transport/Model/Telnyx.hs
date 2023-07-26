{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Buzgibi.Transport.Model.Telnyx 
       (AppRequest (..), 
        Outbound (..),
        AppResponse (..),
        CallRequest (..),
        CallResponse (..),
        encodeCallResponse,
        CallResponseData (..),
        EventType (..),
        Webhook (..),
        Hangup (..),
        encodeHangup,
        CallPayload (..),
        Answered (..),
        encodeAnswered,
        Record (..),
        encodeRecord,
        RecordingStartRequest (..),
        RecordingStartResponse,
        Channels (..),
        Format (..)
       ) where

import Database.Transaction (ParamsShow (..))
import Buzgibi.Transport.Payload (Payload)
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia
import TH.Mk
import Data.Maybe (fromMaybe)
import Data.Text.Extended ()
import Data.Time.Clock (UTCTime)
import GHC.TypeLits (symbolVal, KnownSymbol, Symbol)
import Data.Proxy (Proxy (..))
import Control.Monad (when)

data Outbound =  
     Outbound 
     { outboundChannelLimit :: !(Maybe Int),
       outboundOutboundVoiceProfileId :: !T.Text
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[OmitNothingFields 'True,  FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor Outbound)]]
          Outbound

data AppRequest =
     AppRequest 
     { appRequestApplicationName :: !T.Text, 
       appRequestWebhookEventUrl :: !T.Text,
       appRequestOutbound :: !Outbound 
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

mkEncoder ''CallResponse
mkArbitrary ''CallResponse

encodeCallResponse :: CallResponse -> (T.Text, T.Text, T.Text, T.Text, Bool)
encodeCallResponse = fromMaybe (error "cannot encode CallResponse") . mkEncoderCallResponse

instance ParamsShow CallResponse where
  render = render . encodeCallResponse


data EventType = CallAnswered | CallHangup | RecordingSaved
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[CamelTo2 "_"]]
          EventType


-- webhook
-- {
--     "record_type": "event",
--     "id": "0ccc7b54-4df3-4bca-a65a-3da1ecc777f0",
--     "event_type": "call_bridged",
--     "created_at": "2018-02-02T22:25:27.521992Z",
--     "payload": a json particular to web hook
-- }
data Webhook (s :: Symbol) =
     Webhook 
     { webhookId :: T.Text,
       webhookEventType :: EventType, 
       webhookCreatedAt :: UTCTime,
       webhookPayload :: Object
     }
 
instance KnownSymbol s => FromJSON (Webhook s) where
  parseJSON = withObject "Webhook" $ \o -> do
    webhookRecordType <- o .: "record_type"
    when (webhookRecordType /= symbolVal (Proxy @s)) $ 
      fail $ "record_type isn't one of `" <> symbolVal (Proxy @s) <> "`"
    webhookId <- o .: "id"
    webhookEventType <- o .: "event_type"
    webhookCreatedAt <- o .: "created_at"
    webhookPayload <- o .: "payload"
    pure $ Webhook {..}


data CallPayload = HangupWrapper Hangup | AnsweredWrapper Answered | RecordWrapper Record 

data Hangup =
     Hangup
     { hangupConnectionId :: T.Text,
       hangupFrom :: T.Text,
       hangupTo :: T.Text,
       hangupHangupCause :: T.Text
     } 
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor Hangup)]]
          Hangup

data Answered = 
     Answered 
     { answeredConnectionId :: T.Text,
       answeredCallControlId :: T.Text,
       answeredFrom :: T.Text,
       answeredTo :: T.Text
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor Answered)]]
          Answered

data Record = 
     Record 
     { recordConnectionId :: T.Text,
      -- ID that is unique to the call and can be used to correlate webhook events
       recordCallLegId :: T.Text,
      --  A json object containing the recording URL (ex.: {FORMAT: URL}, where format can be 'mp3' or 'wav'). 
      --  The URL is valid for 10 minutes. After 10 minutes, you may retrieve recordings via 
      --  API using Reports -> Call Recordings documentation, or via Mission Control under Reporting -> Recordings
       recordRecordingUrls :: Payload
     }
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor Record)]]
          Record

mkEncoder ''Hangup
mkArbitrary ''Hangup

mkEncoder ''Answered
mkArbitrary ''Answered

mkEncoder ''Record
mkArbitrary ''Record

encodeHangup :: Hangup -> (T.Text, T.Text, T.Text, T.Text)
encodeHangup = fromMaybe (error "cannot encode Hangup") . mkEncoderHangup

encodeAnswered :: Answered -> (T.Text, T.Text, T.Text, T.Text)
encodeAnswered = fromMaybe (error "cannot encode Answered") . mkEncoderAnswered

encodeRecord :: Record -> (T.Text, T.Text, Payload)
encodeRecord = fromMaybe (error "cannot encode Record") . mkEncoderRecord

instance ParamsShow Hangup where
  render = render . encodeHangup

instance ParamsShow Answered where
  render = render . encodeAnswered

instance ParamsShow Record where
  render = render . encodeRecord

data Format = MP3 | WAV
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Format

data Channels = Single | Dual
      deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Channels

data RecordingStartRequest =
     RecordingStartRequest
     { recordingStartRequestFormat :: Format,
       recordingStartRequestChannels :: Channels
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor RecordingStartRequest)]]
          RecordingStartRequest

data RecordingStartResponse = RecordingStartResponse T.Text

instance FromJSON RecordingStartResponse where
  parseJSON = 
    withObject "RecordingStartResponse" $ \o -> do
      d <- o .: "data"
      fmap RecordingStartResponse $ d .: "result"