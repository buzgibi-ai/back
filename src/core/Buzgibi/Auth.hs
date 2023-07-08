{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Buzgibi.Auth (AuthenticatedUser (..), JWT,  UserIdentClaims, generateJWT, validateJwt, withAuth) where

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
import Control.Monad (unless)
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
import Data.Time.Clock (addUTCTime)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (FromJSON, ToJSON)
import Data.Traversable (for)

data AuthError = NoAuthHeader | NoBearer | TokenInvalid

data JWT

instance HasSecurity JWT where
  securityName _ = "JwtSecurity"
  securityScheme _ = SecurityScheme type_ (Just desc)
    where
      type_ = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
      desc  = "JSON Web Token-based API key"

newtype AuthenticatedUser = AuthenticatedUser {ident :: Int64} deriving (Show)

instance FromJWT AuthenticatedUser where
  decodeJWT _ = Right $ AuthenticatedUser 0

instance ToJWT AuthenticatedUser where
  encodeJWT _ = emptyClaimsSet

instance IsAuth JWT AuthenticatedUser where
  type AuthArgs JWT = '[JWTSettings]
  runAuth _ _ = \cfg -> do
    res <- ask >>= Except.runExceptT . go cfg
    fromEither res
    where 
      go :: JWTSettings -> Request -> Except.ExceptT AuthError AuthCheck AuthenticatedUser
      go cfg req = do 
        header <- Except.except $ maybeToRight NoAuthHeader $ lookup "Authorization" (requestHeaders req)
        let bearer = "Bearer "
        let (mbearer, token) = BS.splitAt (BS.length bearer) header
        unless (mbearer `constEq` bearer) $
          Except.throwE NoBearer 
        usere <- liftIO $ validateJwt cfg token
        Except.except usere
      fromEither (Left NoAuthHeader) = fail "NoAuthHeader"
      fromEither (Left NoBearer) = fail "NoBearer"
      fromEither (Left TokenInvalid) = fail "TokenInvalid"
      fromEither (Right user) = pure user

validateJwt :: JWTSettings -> BS.ByteString -> IO (Either AuthError AuthenticatedUser)
validateJwt cfg@JWTSettings {..} input = do
    keys <- validationKeys
    userClaimSet <- runJOSE @Jose.JWTError $ do
      unverifiedJWT <- Jose.decodeCompact (BSL.fromStrict input)
      Jose.verifyJWT (jwtSettingsToJwtValidationSettings cfg) keys unverifiedJWT
    fmap (first (const TokenInvalid)) $ 
      for userClaimSet $ \UserIdentClaims {userIdentClaimsIdent} -> 
        return $ AuthenticatedUser userIdentClaimsIdent

withAuth :: AuthResult AuthenticatedUser -> (AuthenticatedUser -> KatipControllerM (Response a)) -> KatipControllerM (Response a)
withAuth (Authenticated user) runApi = runApi user
withAuth e _ = return $ Error $ asError @T.Text $ "only for authorized personnel, error: " <> mkError e
  where 
      mkError BadPassword = "wrong password"
      mkError NoSuchUser = "no user found"
      mkError Indefinite = "an authentication procedure cannot be carried out"

generateJWT :: Jose.JWK -> Int64 -> IO (Either Jose.JWTError BSL.ByteString)
generateJWT jwk ident = do
  t <- currentTime
  let claims = 
       Jose.emptyClaimsSet
       & Jose.claimExp ?~ Jose.NumericDate (addUTCTime 864000 t)
       & Jose.claimIat ?~ Jose.NumericDate t
  let user = UserIdentClaims claims ident
  Jose.runJOSE $ do
    alg <- Jose.bestJWSAlg jwk
    fmap encodeCompact $ Jose.signJWT jwk (Jose.newJWSHeader ((), alg)) user

data UserIdentClaims = 
     UserIdentClaims 
     { 
        userIdentClaimsJwtClaims :: Jose.ClaimsSet, 
        userIdentClaimsIdent :: Int64 
     }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor UserIdentClaims)]]
          UserIdentClaims

instance Jose.HasClaimsSet UserIdentClaims where
  claimsSet f s = fmap (\a' -> s { userIdentClaimsJwtClaims = a' }) (f (userIdentClaimsJwtClaims s))
