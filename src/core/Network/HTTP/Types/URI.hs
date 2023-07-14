{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.URI (extractPath) where

import qualified Data.ByteString as B

-- | Extract whole path (path segments + query) from a
-- <http://tools.ietf.org/html/rfc2616#section-5.1.2 RFC 2616 Request-URI>.
--
-- >>> extractPath "/path"
-- "/path"
--
-- >>> extractPath "http://example.com:8080/path"
-- "/path"
--
-- >>> extractPath "http://example.com"
-- "/"
--
-- >>> extractPath ""
-- "/"
extractPath :: B.ByteString -> B.ByteString
extractPath = ensureNonEmpty . extract
  where
    extract path
      | "http://"  `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 7) path
      | "https://" `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 8) path
      | otherwise = path
    breakOnSlash = B.break (== 47)
    ensureNonEmpty "" = "/"
    ensureNonEmpty p  = p