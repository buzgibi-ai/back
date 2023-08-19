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
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

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
        Format (..),
        HangupCause (..),
        MachineDetection (..),
        MachineDetectionResult (..),
        HangupRequest (..),
        HangupResponse,
        PlaybackStartRequest (..),
        PlaybackStartResponse,
        BalanceResponse (..),
        BalanceResponseWrapper (..),
        Errors (..),
        Error (..)
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
import Data.String.Conv
import qualified Data.Vector as V

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
      callRequestAudioUrl :: !T.Text,
       -- When a call is answered, Telnyx runs real-time detection to determine 
       -- if it was picked up by a human or a machine and sends an call.machine.detection.ended webhook with the analysis result
      callRequestAnsweringMachineDetection :: !T.Text
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
      -- ID that is unique to the call and can be used to correlate webhook events
      callResponseCallLegId :: !T.Text,
      -- Unique identifier and token for controlling the call.
      callResponseCallControlId :: !T.Text
     } 
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor CallResponse)]]
          CallResponse

mkEncoder ''CallResponse
mkArbitrary ''CallResponse

encodeCallResponse :: CallResponse -> (T.Text, T.Text)
encodeCallResponse = fromMaybe (error "cannot encode CallResponse") . mkEncoderCallResponse

instance ParamsShow CallResponse where
  render = render . encodeCallResponse


data EventType = 
     CallAnswered | 
     CallHangup | 
     CallRecordingSaved | 
     CallInitiated | 
     CallMachineDetectionEnded |
     CallPlaybackStarted |
     CallPlaybackEnded
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[CamelTo2 "."]]
          EventType


-- webhook
-- {
--     "id": "0ccc7b54-4df3-4bca-a65a-3da1ecc777f0",
--     "event_type": "call_bridged",
--     "created_at": "2018-02-02T22:25:27.521992Z",
--     "payload": a json particular to web hook
-- }
data Webhook =
     Webhook 
     { webhookId :: T.Text,
       webhookEventType :: EventType, 
       webhookCreatedAt :: UTCTime,
       webhookPayload :: Object
     }
 
instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o -> do
    dataObj <- o .: "data"
    webhookId <- dataObj .: "id"
    webhookEventType <- dataObj .: "event_type"
    webhookCreatedAt <- dataObj .: "occurred_at"
    webhookPayload <- dataObj .: "payload"
    pure $ Webhook {..}

data HangupCause = 
    -- Callee chose not to accept this call. The callee is not busy nor incompatible
    CallRejected | 
    -- Call is being cleared because one of the users involved in the call has requested that the call be cleared.
    NormalClearing | 
    -- SIP request has been terminated by a bye or cancel.
    OriginatorCancel |
    -- Call was canceled because the destination channel took too long to answer
    Timeout |
    -- Call lasted the maximum allowable duration of 4 hours or a custom time limit configured in the Dial or Transfer call commands.
    TimeLimit |
    -- Called party is unable to accept another call because the user busy condition has been encountered.
    UserBusy |
    -- Called party cannot be reached because the valid destination number is not currently allocated (assigned).
    NotFound |
    -- Unexpected hangup not matching any of the above causes. `sip_hangup_cause` contained in the webhook may contain additional information.
    Unspecified
    deriving stock (Generic, Show, Eq, Ord)
    deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[CamelTo2 "_"]]
          HangupCause

mkArbitrary ''HangupCause

instance ParamsShow HangupCause where
    render = toS . encode

data CallPayload = 
     HangupWrapper Hangup | 
     AnsweredWrapper Answered | 
     RecordWrapper Record |
     Skip EventType |
     MachineDetectionWrapper MachineDetection 

data Hangup =
     Hangup
     { hangupConnectionId :: T.Text,
       hangupCallLegId :: T.Text,
       hangupHangupCause :: HangupCause
     } 
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor Hangup)]]
          Hangup

data Answered = Answered { answeredCallControlId :: T.Text, answeredCallLegId :: T.Text }
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

data MachineDetectionResult = Human | Machine | NotSure
    deriving stock (Generic, Show, Eq, Ord)
    deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier '[CamelTo2 "_"]]
          MachineDetectionResult

data MachineDetection =
     MachineDetection
     { machineDetectionCallLegId :: T.Text,
       machineDetectionResult :: MachineDetectionResult,
       machineDetectionConnectionId :: T.Text,
       machineDetectionCallControlId :: T.Text,
       machineDetectionClientState :: Maybe T.Text
     }
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor MachineDetection)]]
          MachineDetection

data HangupRequest = HangupRequest { hangupRequestClientState :: Maybe T.Text } 
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor HangupRequest)]]
          HangupRequest

data HangupResponse

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

data PlaybackStartRequest = 
     PlaybackStartRequest
     { playbackStartRequestAudioUrl :: T.Text
     }  
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor PlaybackStartRequest)]]
          PlaybackStartRequest

data PlaybackStartResponse = PlaybackStartResponse T.Text

instance FromJSON PlaybackStartResponse where
  parseJSON = 
    withObject "PlaybackStartResponse" $ \o -> do
      d <- o .: "data"
      fmap PlaybackStartResponse $ d .: "result"

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

mkEncoder ''Hangup
mkArbitrary ''Hangup

mkEncoder ''Answered
mkArbitrary ''Answered

mkEncoder ''Record
mkArbitrary ''Record

encodeHangup :: Hangup -> (T.Text, T.Text, HangupCause)
encodeHangup = fromMaybe (error "cannot encode Hangup") . mkEncoderHangup

encodeAnswered :: Answered -> (T.Text, T.Text)
encodeAnswered = fromMaybe (error "cannot encode Answered") . mkEncoderAnswered

encodeRecord :: Record -> (T.Text, T.Text, Payload)
encodeRecord = fromMaybe (error "cannot encode Record") . mkEncoderRecord

instance ParamsShow Hangup where
  render = render . encodeHangup

instance ParamsShow Answered where
  render = render . encodeAnswered

instance ParamsShow Record where
  render = render . encodeRecord

data BalanceResponse = 
     BalanceResponse
     { balanceResponseAvailableCredit :: T.Text,
       balanceResponseBalance :: T.Text,
       balanceResponseCreditLimit :: T.Text,
       balanceResponseCurrency :: T.Text,
       balanceResponseRecordType :: T.Text
     }
     deriving stock (Generic)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor BalanceResponse)]]
          BalanceResponse 

newtype BalanceResponseWrapper = BalanceResponseWrapper BalanceResponse

instance FromJSON BalanceResponseWrapper where
  parseJSON = 
    withObject "BalanceResponseWrapper" $ \o -> do
      _data <- o .: "data"
      fmap BalanceResponseWrapper $ parseJSON @BalanceResponse _data


data Error = Error { errorCode :: Int, errorDetail :: T.Text }
     deriving stock (Generic)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Error)]]
          Error 

newtype Errors = Errors [Error]

instance FromJSON Errors where
  parseJSON =
    withObject "Errors" $ \o -> do
      xs <- o .: "errors"
      fmap (Errors . V.toList) $ withArray "Errors(array)" (traverse parseJSON) xs