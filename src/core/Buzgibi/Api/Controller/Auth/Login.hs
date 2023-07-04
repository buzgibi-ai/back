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

data Error = User404 | JWT

instance Show Error where 
    show User404 = "user or password wrong"
    show JWT = "jwt generation error"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  token <- liftIO $ Auth.generateJWT key $ email cred
  resp <- fmap (join . first (const JWT)) $ for token $ \tk -> do
    resp <- transactionM hasql $
      statement Auth.insertToken 
       (email cred, password cred, tk^.bytesLazy.from textbs)
    return $
      if resp then Right $ tk^.bytesLazy.from textbs
      else Left User404
  return $ withError resp AuthToken
