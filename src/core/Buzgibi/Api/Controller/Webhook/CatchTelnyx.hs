{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Transport.Model.Telnyx
import  Buzgibi.Statement.User.Survey (insertAppPhone, insertVoiceUrlAppPhone)
import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))
import Data.Aeson (encode, eitherDecode)
import Data.Traversable (for)
import Control.Monad (when)
import Data.Either (isLeft)
import Database.Transaction
import Control.Lens
import Data.Aeson.Types (Value (Object))
import Data.Coerce (coerce)

controller :: Payload -> KatipControllerM ()
controller Payload {..} = do
  let parseRes = do 
        Webhook {..} <- eitherDecode @(Webhook "event") $ encode getPayload
        case webhookEventType of 
          CallAnswered -> fmap AnsweredWrapper $ eitherDecode @Answered $ encode webhookPayload
          CallHangup -> fmap HangupWrapper $ eitherDecode @Hangup $ encode webhookPayload
          RecordingSaved -> fmap RecordWrapper $ eitherDecode @Record $ encode webhookPayload
  res :: Either String () 
    <- for parseRes $ \case
         HangupWrapper hangup -> do
           hasql <- fmap (^. katipEnv . hasqlDbPool) ask
           transactionM hasql $ statement insertAppPhone $ encodeHangup hangup
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: hangup received " <> show hangup
         AnsweredWrapper _ -> undefined
         RecordWrapper record -> do
           hasql <- fmap (^. katipEnv . hasqlDbPool) ask
           transactionM hasql $ statement insertVoiceUrlAppPhone (encodeRecord record & _3 %~ (Object . coerce))
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: record received " <> show record
  when (isLeft res) $ $(logTM) ErrorS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: parse error: " <> show res