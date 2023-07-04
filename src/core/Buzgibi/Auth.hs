{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Buzgibi.Auth (AuthenticatedUser (..), JWT,  generateJWT, withAuth) where

import Buzgibi.Transport.Response

import Crypto.JWT
import qualified Data.Text as T
import KatipController (KatipControllerM)
import Servant.Auth.Server (AuthResult (..), FromJWT (decodeJWT), ToJWT (..), JWTSettings (..))
import Servant.Auth.Server.Internal.Class (IsAuth (..))
import Data.Int (Int64)
import Servant.Auth.Swagger (HasSecurity (..))
import Data.Swagger (SecurityScheme (..),  SecuritySchemeType( SecuritySchemeApiKey ), ApiKeyParams (..),  ApiKeyLocation( ApiKeyHeader ))
import Control.Monad.Reader (ask)
import Network.Wai (requestHeaders)
import qualified Control.Monad.Trans.Except as Except
import Data.Either.Combinators (maybeToRight)
import qualified Data.ByteString as BS
import Control.Monad (unless, join)
import Data.ByteArray (constEq)
import Servant.Auth.Server.Internal.Types (AuthCheck (..))
import Network.Wai (Request)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.JOSE as Jose
import qualified Crypto.JWT as Jose
import qualified Data.ByteString.Lazy as BSL
import Servant.Auth.Server.Internal.ConfigTypes (jwtSettingsToJwtValidationSettings)
import Data.Bifunctor (first)
import Control.Lens
import Control.Monad.Time (currentTime)
import Data.String (fromString)
import Data.Time.Clock (addUTCTime)

data AuthError = NoAuthHeader | NoBearer | TokenInvalid | JWTError

data JWT

instance HasSecurity JWT where
  securityName _ = "JwtSecurity"
  securityScheme _ = SecurityScheme type_ (Just desc)
    where
      type_ = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
      desc  = "JSON Web Token-based API key"

data AuthenticatedUser = AuthenticatedUser {ident :: Int64, email :: T.Text} deriving (Show)

instance FromJWT AuthenticatedUser where
  decodeJWT _ = Right $ AuthenticatedUser 0 ""

instance ToJWT AuthenticatedUser where
  encodeJWT _ = emptyClaimsSet

instance IsAuth JWT AuthenticatedUser where
  type AuthArgs JWT = '[JWTSettings]
  runAuth _ _ = \cfg -> do
    res <- ask >>= Except.runExceptT . validateJwt cfg
    fromEither res
    where 
      validateJwt :: JWTSettings -> Request -> Except.ExceptT AuthError AuthCheck AuthenticatedUser
      validateJwt cfg req = do 
        header <- Except.except $ maybeToRight NoAuthHeader $ lookup "Authorization" (requestHeaders req)
        let bearer = "Bearer "
        let (mbearer, token) = BS.splitAt (BS.length bearer) header
        unless (mbearer `constEq` bearer) $
          Except.throwE NoBearer 
        usere <- liftIO $ go cfg token
        Except.except usere
      fromEither (Left NoAuthHeader) = fail "NoAuthHeader"
      fromEither (Left NoBearer) = fail "NoBearer"
      fromEither (Left TokenInvalid) = fail "TokenInvalid"
      fromEither (Right user) = pure user
      go cfg@JWTSettings {..} input = do
         keys <- validationKeys
         verifiedJWT <- runJOSE @Jose.JWTError $ do
           unverifiedJWT <- Jose.decodeCompact (BSL.fromStrict input)
           Jose.verifyClaims (jwtSettingsToJwtValidationSettings cfg) keys unverifiedJWT   
         pure $ join $ bimap (const TokenInvalid) (first (const JWTError) . decodeJWT) verifiedJWT

withAuth :: AuthResult AuthenticatedUser -> (AuthenticatedUser -> KatipControllerM (Response a)) -> KatipControllerM (Response a)
withAuth (Authenticated user) runApi = runApi user
withAuth e _ = return $ Error $ asError @T.Text $ "only for authorized personnel, error: " <> mkError e
  where 
      mkError BadPassword = "wrong password"
      mkError NoSuchUser = "no user found"
      mkError Indefinite = "an authentication procedure cannot be carried out"

generateJWT :: Jose.JWK -> T.Text -> IO (Either Jose.JWTError BSL.ByteString)
generateJWT jwk email = do
  t <- currentTime
  let claims = 
       Jose.emptyClaimsSet
       & Jose.claimIss ?~ fromString (T.unpack email)
       & Jose.claimExp ?~ Jose.NumericDate (addUTCTime 864000 t)
       & Jose.claimIat ?~ Jose.NumericDate t
  Jose.runJOSE $ do
    alg <- Jose.bestJWSAlg jwk
    fmap encodeCompact $ Jose.signClaims jwk (Jose.newJWSHeader ((), alg)) claims