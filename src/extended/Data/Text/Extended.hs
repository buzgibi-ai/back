module Data.Text.Extended (UnicodeText (..), module T) where

import Data.Default.Class
import qualified Data.Text as T
import Test.QuickCheck.Extended

-- | Wrapper for text in order to generate valid unicode string
newtype UnicodeText = UnicodeText T.Text

instance Arbitrary UnicodeText where arbitrary = UnicodeText <$> genText

instance Default T.Text where
  def = T.pack ""
