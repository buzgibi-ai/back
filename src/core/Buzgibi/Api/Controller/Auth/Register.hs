{-# LANGUAGE OverloadedStrings #-}

module Buzgibi.Api.Controller.Auth.Register (controller) where

import qualified Buzgibi.Auth as Auth
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Model.User (AuthToken (..), Credentials (email, password))
import Buzgibi.Transport.Response (Response)
import Buzgibi.Api.Controller.Utils (withError)
import Control.Lens
import Control.Monad.IO.Class
import Database.Transaction
import KatipController
import Control.Lens.Iso.Extended
import Data.Traversable (for)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)

data Error = UserTaken | JWT

instance Show Error where
  show UserTaken = "user is taken"
  show JWT = "jwt generation failed"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident = liftIO . Auth.generateJWT key ident
  res <- fmap join $ transactionM hasql $ do 
    identm <- statement Auth.insertUser (email cred, password cred)
    fmap (maybeToRight UserTaken) $ for identm $ \ident -> do 
      tokene <- mkToken ident $ email cred
      fmap (first (const JWT)) $ 
        for tokene $ \tokenbs -> do
          let token = tokenbs^.bytesLazy.from textbs
          _ <- statement Auth.insertJwt (ident, token)
          return token
  return $ withError res AuthToken