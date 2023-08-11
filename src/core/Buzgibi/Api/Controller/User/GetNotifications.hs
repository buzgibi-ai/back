module Buzgibi.Api.Controller.User.GetNotifications (controller) where

import Buzgibi.Statement.User.Notification (GetNotification, get)
import Buzgibi.Api.Controller.Utils (withError)
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import Katip.Controller
import Database.Transaction
import Data.Coerce (coerce)
import Control.Lens
import Data.Aeson (eitherDecode, encode)

controller :: AuthenticatedUser -> KatipControllerM (Response [GetNotification])
controller user = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask 
  fmap (flip withError id . sequence . map (eitherDecode . encode)) $ transactionM hasql $ statement get $ coerce user