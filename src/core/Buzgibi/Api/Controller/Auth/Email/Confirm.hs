{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Buzgibi.Api.Controller.Auth.Email.Confirm (controller) where

import Buzgibi.Auth (AuthenticatedUser (..))
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Response (Response (Ok))
import Control.Lens
import Database.Transaction
import Katip.Controller
import Data.Text (Text)

controller :: AuthenticatedUser -> Maybe Text -> KatipControllerM (Response Bool)
controller _ Nothing = return $ Ok False
controller (AuthenticatedUser {ident}) (Just key) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap Ok $ transactionM hasql $ statement Auth.confirmEmail (ident, key)