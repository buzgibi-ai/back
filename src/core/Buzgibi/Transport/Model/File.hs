{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buzgibi.Transport.Model.File (Hash (..), Name (..), Mime (..), UnicodeText (..), Bucket (..)) where

import Data.Text.Extended
import Database.Transaction
import Test.QuickCheck.Extended
import Control.Lens
import Control.Lens.Iso.Extended

instance ParamsShow UnicodeText where render = (^. coerced . from stext)

newtype Hash = Hash UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Name = Name UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Mime = Mime UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Bucket = Bucket UnicodeText
  deriving newtype (Arbitrary, ParamsShow)