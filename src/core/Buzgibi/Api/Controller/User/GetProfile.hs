module Buzgibi.Api.Controller.User.GetProfile (controller) where

import Buzgibi.Auth (AuthenticatedUser)
import Buzgibi.Transport.Response
import Katip.Controller (KatipControllerM)

controller :: AuthenticatedUser -> KatipControllerM (Response ())
controller _ = undefined
