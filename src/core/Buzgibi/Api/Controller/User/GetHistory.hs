module Buzgibi.Api.Controller.User.GetHistory (controller) where

import Buzgibi.Auth (AuthenticatedUser) 
import Buzgibi.Transport.Response
import KatipController (KatipControllerM)


controller :: AuthenticatedUser -> KatipControllerM (Response ())
controller _ = undefined