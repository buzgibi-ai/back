#!/usr/bin/env stack
-- stack script --resolver lts-21.4 --package jose --package aeson  --package bytestring
{-# LANGUAGE OverloadedStrings #-}

import Crypto.JOSE.JWK
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

main = do
 [file] <- getArgs
 jwk <- fmap encode $ genJWK (RSAGenParam (4096 `div` 8))
 B.writeFile file jwk