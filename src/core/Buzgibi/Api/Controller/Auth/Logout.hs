{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Api.Controller.Auth.Logout (controller) where

import Buzgibi.Api.Controller.Utils (withError)
import Buzgibi.Auth (AuthenticatedUser (..))
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Response (Response)
import Control.Lens
import Database.Transaction
import Katip
import Katip.Controller

data Error = User404

instance Show Error where
  show User404 = "user or password wrong or jwt is invalid"

controller :: AuthenticatedUser -> KatipControllerM (Response ())
controller AuthenticatedUser {ident} = do
  $(logTM) DebugS (logStr ("user ident ---> " <> show ident))
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  resp <- transactionM hasql $ do
    resp <- statement Auth.logout ident
    return $
      if resp
        then Right ()
        else Left User404
  return $ withError resp id
