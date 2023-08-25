{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hash (mkHash, mkHash512, mkHashWhirlpool) where

import "hashing" Crypto.Hash
import Data.Text
import Data.String.Conv (toS)

mkHashG :: forall al a . (HashAlgorithm  al, Show al, Show a) => a -> Text
mkHashG x = toS $ show (hash (toS (show x)) :: al)

mkHash :: Show a => a -> Text
mkHash = mkHashG @SHA256

mkHash512 :: Show a => a -> Text
mkHash512 = mkHashG @SHA512

mkHashWhirlpool :: Show a => a -> Text
mkHashWhirlpool = mkHashG @Whirlpool