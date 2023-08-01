{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Api.Controller.Webhook.CatchGit (controller) where

import Buzgibi.Transport.Payload (Payload (..))
import BuildInfo (location)
import Katip.Controller
import Katip

controller :: Payload -> KatipControllerM ()
controller payload = $(logTM) DebugS $ logStr $ $location <> " catch git webhook, payload ---> " <> show payload