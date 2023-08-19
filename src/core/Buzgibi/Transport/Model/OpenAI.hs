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

module Buzgibi.Transport.Model.OpenAI 
       (TranscriptionResponse (..), 
        SARequest (..),
        SAResponse (..),
        Error (..), 
        defSARequest
       ) where

import GHC.Generics (Generic)
import Data.Aeson hiding (Error)
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia
import qualified Data.Vector as V

data TranscriptionResponse = TranscriptionResponse { transcriptionResponseText :: T.Text }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor TranscriptionResponse)]]
          TranscriptionResponse

-- {
--   "model": "text-davinci-003",
--   "prompt": "Say this is a test",
--   "max_tokens": 7,
--   "temperature": 0,
--   "top_p": 1,
--   "n": 1,
--   "stream": false,
--   "logprobs": null,
--   "stop": "\n"
-- }
data SARequest = 
     SARequest
     { sARequestModel :: T.Text,
       sARequestPrompt :: T.Text,
       sARequestTemperature :: Double,
       sARequestMaxTokens :: Int
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor SARequest)]]
          SARequest

defSARequest :: SARequest
defSARequest = 
  SARequest
  { sARequestModel = "text-davinci-003",
    sARequestPrompt = mempty,
    sARequestTemperature = 0,
    sARequestMaxTokens = 7
  }

-- {
--   "id": "cmpl-uqkvlQyYK7bGYrRHQ0eXlWi7",
--   "object": "text_completion",
--   "created": 1589478378,
--   "model": "text-davinci-003",
--   "choices": [
--     {
--       "text": "\n\nThis is indeed a test",
--       "index": 0,
--       "logprobs": null,
--       "finish_reason": "length"
--     }
--   ],
--   "usage": {
--     "prompt_tokens": 5,
--     "completion_tokens": 7,
--     "total_tokens": 12
--   }
-- }
newtype SAResponse = SAResponse [T.Text]

instance FromJSON SAResponse where
  parseJSON = withObject "SAResponse" $ \o -> do
    choices <- o .: "choices"
    fmap (SAResponse . V.toList) $ 
      withArray "SAResponse(array)" (traverse getText) choices
    where
       getText = withObject "SAResponse(array):item" (flip (.:) "text")


newtype Error = Error T.Text

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o -> do
    e <- o .: "error"
    fmap Error $ e .: "code"