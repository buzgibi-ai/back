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

module Buzgibi.Transport.Model.Deepgram (Result (..), Alternatives (..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), Value (Array, Object))
import qualified Data.Text as T
import Data.Aeson.Generic.DerivingVia
import qualified Data.Vector as V
import Data.Traversable (for)
import Data.Maybe (fromMaybe)

data Alternatives = Alternatives { alternativesTranscript :: T.Text , alternativesConfidence :: Double }
    deriving stock (Generic, Show)
     deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Alternatives)]]
          Alternatives

newtype Result = Result [Alternatives]
  deriving Show

instance FromJSON Result where
  parseJSON = withObject "Result" $ \o -> do
    r <- o .: "results"
    Array xs <- r .: "channels"
    fmap (fromMaybe (Result [])) $ 
      for (xs V.!? 0) $ \(Object x) -> do
        ys <- x .: "alternatives"
        fmap Result $ parseJSON @[Alternatives] ys