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

data Error = UserTaken | JWT

instance Show Error where
  show UserTaken = "user is taken"
  show JWT = "jwt generation failed"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  token <- liftIO $ Auth.generateJWT key $ email cred
  res <- fmap (join . first (const JWT)) $ for token $ \tk -> do
    res <- transactionM hasql $ 
      statement Auth.insert
        (email cred, password cred, tk^.bytesLazy.from textbs)
    return $ 
      if res then Right $ tk^.bytesLazy.from textbs
      else Left UserTaken
  return $ withError res AuthToken