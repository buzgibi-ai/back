{-# LANGUAGE OverloadedStrings #-}

module Buzgibi.Api.Controller.Auth.Login (controller) where

import qualified Buzgibi.Auth as Auth
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Model.User (AuthToken (..), Credentials (email, password))
import Buzgibi.Transport.Response (Response)
import Buzgibi.Api.Controller.Utils (withError)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (join)
import Database.Transaction
import KatipController
import Control.Lens.Iso.Extended
import Data.Traversable (for)
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)

data Error = User404 | JWT | WrongPass

instance Show Error where 
    show User404 = "user wrong"
    show JWT = "jwt generation error"
    show WrongPass = "wrong pass"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken = liftIO . Auth.generateJWT key
  res <- fmap join $ transactionM hasql $ do 
    identm <- statement Auth.getUserIdByEmail (email cred)
    fmap (maybeToRight User404) $ 
      for identm $ \ident -> do 
        tokene <- mkToken ident
        fmap (join . first (const JWT)) $ 
          for tokene $ \tokenbs -> do
            let token = tokenbs^.bytesLazy.from textbs
            res <- statement Auth.insertToken  (email cred, password cred, token)
            return $ if res then Right token
                     else Left WrongPass
  return $ withError res AuthToken