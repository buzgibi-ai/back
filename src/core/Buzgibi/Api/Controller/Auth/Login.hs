{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.Auth.Login (controller) where

import Buzgibi.Api.Controller.Utils (withError)
import qualified Buzgibi.Auth as Auth
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Model.User (AuthToken (..), Credentials (email, password))
import Buzgibi.Transport.Response (Response)
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Traversable (for)
import Database.Transaction
import Katip.Controller
import qualified Data.Hashable as H (hash)
import Data.Int (Int64)

data Error = User404 | JWT | WrongPass

instance Show Error where
  show User404 = "user wrong"
  show JWT = "jwt generation error"
  show WrongPass = "wrong pass"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident = liftIO . Auth.generateJWT key ident
  res <- fmap join $ transactionM hasql $ do
    identm <- statement Auth.getUserIdByEmail (email cred)
    fmap (maybeToRight User404) $
      for identm $ \ident -> do
        tokene <- mkToken ident $ email cred
        fmap (join . first (const JWT)) $
          for tokene $ \(tokenbs, uuid) -> do
            let token = tokenbs ^. bytesLazy . from textbs
            let uuid_hash = fromIntegral @_ @Int64 $ H.hash uuid
            res <- statement Auth.insertToken (email cred, password cred, token, uuid_hash)
            return $
              if res
                then Right token
                else Left WrongPass
  return $ withError res AuthToken
