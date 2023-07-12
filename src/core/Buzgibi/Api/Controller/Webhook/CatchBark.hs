module Buzgibi.Api.Controller.Webhook.CatchBark (controller) where

import Buzgibi.Transport.Payload (Payload)
import Buzgibi.Transport.Response
import Katip.Controller (KatipControllerM)

controller :: Payload -> KatipControllerM (Response ())
controller _ = undefined
