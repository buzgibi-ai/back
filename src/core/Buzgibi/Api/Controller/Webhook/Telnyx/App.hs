{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.Webhook.Telnyx.App (controller) where

import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))

controller :: Payload -> KatipControllerM ()
controller payload = $(logTM) DebugS (logStr @String ("catch bark webhook, payload ---> " <> show payload))