{-# LANGUAGE OverloadedStrings #-}

module Buzgibi.Api.Controller.Auth.Logout (controller) where

import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Model.User (AuthToken (..))
import Buzgibi.Transport.Response (Response)
import Buzgibi.Api.Controller.Utils (withError)
import Control.Lens
import Database.Transaction
import KatipController
import Data.Coerce

data Error = Token404

instance Show Error where 
    show Token404 = "user or password wrong"

controller :: AuthToken -> KatipControllerM (Response ())
controller token = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  resp <- transactionM hasql $ do
    resp <- statement Auth.logout $ coerce token 
    return $
      if resp then Right ()
      else Left Token404
  return $ withError resp id
