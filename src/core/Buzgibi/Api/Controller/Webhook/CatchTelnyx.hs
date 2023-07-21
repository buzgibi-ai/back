{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Transport.Model.Telnyx (Webhook)
import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))
import Data.Aeson (encode, eitherDecode)

controller :: Payload -> KatipControllerM ()
controller Payload {..} = do
  let res = eitherDecode @(Webhook "event" Int) $ encode getPayload
  case res of 
    Right webhook -> $(logTM) DebugS (logStr @String ("catch bark webhook, payload ---> " <> show webhook))
    Left error -> $(logTM) ErrorS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: webhook parse has failed " <> error