{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Buzgibi.Transport.Model.Bark (Response (..), Status (..)) where


import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia
import qualified Data.Map as M

data Status = 
     -- starting: the prediction is starting up. 
     --- If this status lasts longer than a few seconds, 
     -- then it's typically because a new worker is being started to run the prediction.
     Starting |
     -- processing: the model is currently running.
     Processing |
     -- succeeded: the prediction completed successfully.
     Succeeded |
     -- failed: the prediction encountered an error during processing.
     Failed |
     -- canceled: the prediction was canceled by the user.
     Canceled
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Status

-- {
--   "id": "rrr4z55ocneqzikepnug6xezpe",
--   "version": "be04660a5b93ef2aff61e3668dedb4cbeb14941e62a3fd5998364a32d613e35e",
--   "urls": {
--     "get": "https://api.replicate.com/v1/predictions/rrr4z55ocneqzikepnug6xezpe",
--     "cancel": "https://api.replicate.com/v1/predictions/rrr4z55ocneqzikepnug6xezpe/cancel"
--   },
--   "created_at": "2022-09-13T22:54:18.578761Z",
--   "started_at": "2022-09-13T22:54:19.438525Z",
--   "completed_at": "2022-09-13T22:54:23.236610Z",
--   "source": "api",
--   "status": "succeeded",
--   "input": {
--     "prompt": "oak tree with boletus growing on its branches"
--   },
--   "output": [
--      "audio_out": ....
--   ],
--   "error": null,
--   "logs": "Using seed: 36941...",
--   "metrics": {
--     "predict_time": 4.484541
--   }
-- }
data Response = 
     Response
     { responseIdent :: !T.Text,
       responseVersion :: !T.Text, 
       responseStatus :: !Status, 
       responseInput :: !T.Text,
       responseOutput :: !(Maybe (M.Map T.Text T.Text))
     }
     deriving Generic
     deriving ToJSON
     deriving Show

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    responseIdent <- o .: "id"
    responseVersion <- o .: "version"
    responseStatus <- o .: "status"
    input <- o .: "input" 
    responseInput <- input .: "prompt"
    responseOutput <- o .:? "output"
    pure $ Response {..}