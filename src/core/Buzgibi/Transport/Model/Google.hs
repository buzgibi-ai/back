{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buzgibi.Transport.Model.Google (TranscribeVoiceRequest (..), TranscribeVoiceResponse (..), Audio (..), defConfig, Result (..), Alternatives (..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, withArray, (.:), (.:?))
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia
import qualified Data.Vector as V
import Data.Traversable (for)

-- https://cloud.google.com/speech-to-text/docs/reference/rest/v1/RecognitionConfig
data Config =
     Config 
     {
       -- Maximum number of recognition hypotheses to be returned. 
       -- Specifically, the maximum number of SpeechRecognitionAlternative messages within each SpeechRecognitionResult. 
       -- The server may return fewer than maxAlternatives. Valid values are 0-30. A value of 0 or 1 will return a maximum of one. If omitted, will return a maximum of one.
        configMaxAlternatives :: Int,
        -- Which model to select for the given request. 
        -- Select the model best suited to your domain to get best results. 
        -- If a model is not explicitly specified, then we auto-select a model based on the parameters in the RecognitionConfig
        configModel :: T.Text,
        -- Set to true to use an enhanced model for speech recognition. 
        -- If useEnhanced is set to true and the model field is not set, then an appropriate enhanced model is chosen if an enhanced model exists for the audio.
        -- If useEnhanced is true and an enhanced version of the specified model does not exist, then the speech is recognized using the standard version of the specified model.
        configUseEnhanced :: Bool,
        -- Contains the language + region/locale to use for speech recognition of the supplied audio. The language code must be a BCP-47 identifier
        configLanguageCode :: T.Text,
        configAudioChannelCount :: Int
     }
    deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor Config)]]
          Config

defConfig :: Config
defConfig = Config 20 "default" True "en-EN" 2

data Audio = Audio { audioContent :: T.Text }
    deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor Audio)]]
          Audio

data TranscribeVoiceRequest =
     TranscribeVoiceRequest
     {
        transcribeVoiceRequestConfig :: Config,
        transcribeVoiceRequestAudio :: Audio
     }
    deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor TranscribeVoiceRequest)]]
          TranscribeVoiceRequest

data Alternatives = Alternatives { alternativesTranscript :: T.Text , alternativesConfidence :: Double }
    deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Alternatives)]]
          Alternatives

newtype Result = Result [Alternatives]
  deriving Show

instance FromJSON Result where
  parseJSON = withObject "Result" $ \o -> do
    alt <- o .: "alternatives"
    fmap (Result . V.toList) $ withArray "Result(array)" (traverse (parseJSON @Alternatives)) alt

data TranscribeVoiceResponse =
     TranscribeVoiceResponse
     {
         transcribeVoiceResponseResults :: Maybe [Result],
         transcribeVoiceResponseTotalBilledTime :: Maybe T.Text
     }
     deriving Show

instance FromJSON TranscribeVoiceResponse where
  parseJSON = withObject "TranscribeVoiceResponse" $ \o -> do
    results <- o .:? "results"
    transcribeVoiceResponseResults <- for results $ fmap V.toList . withArray "TranscribeVoiceResponse(array)" (traverse (parseJSON @Result))
    transcribeVoiceResponseTotalBilledTime <- o .: "totalBilledTime"
    pure TranscribeVoiceResponse {..}