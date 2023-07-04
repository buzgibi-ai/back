{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Buzgibi.Auth (AuthenticatedUser (..), withBasicAuth, checkBasicAuth) where

import Buzgibi.Transport.Response

import Control.Monad.Except
import Crypto.JWT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Katip
import Katip.Core
import KatipController (KatipControllerM, KatipLoggerLocIO)
import Servant (BasicAuthData (..))
import Servant.Auth.Server (AuthResult (..), BasicAuthCfg, FromBasicAuthData (..), FromJWT (decodeJWT), ToJWT (..), wwwAuthenticatedErr)

newtype AuthenticatedUser = AuthenticatedUser {email :: T.Text} deriving (Show)

instance FromJWT AuthenticatedUser where
  decodeJWT _ = undefined

instance ToJWT AuthenticatedUser where
  encodeJWT _ = emptyClaimsSet

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData basicAuthData authChecker = authChecker basicAuthData

withBasicAuth :: AuthResult AuthenticatedUser -> (AuthenticatedUser -> KatipControllerM (Response a)) -> KatipControllerM (Response a)
withBasicAuth (Authenticated user) runApi = runApi user
withBasicAuth _ _ = throwError $ wwwAuthenticatedErr "only for authorized personnel"

checkBasicAuth :: KatipLoggerLocIO -> M.Map T.Text AuthenticatedUser -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
checkBasicAuth log storage auth_data = do
  log getLoc InfoS $ ls $ show (basicAuthPassword auth_data, basicAuthUsername auth_data)
  return $ maybe Indefinite (const (Authenticated (AuthenticatedUser mempty))) $ T.decodeUtf8 (basicAuthPassword auth_data) `M.lookup` storage
