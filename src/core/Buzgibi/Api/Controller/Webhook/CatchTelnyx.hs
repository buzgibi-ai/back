{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Transport.Model.Telnyx
import qualified Buzgibi.Statement.User.Survey as User.Survey (insertAppPhoneCall, updateAppPhoneCall, CallStatus (..))
import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))
import Data.Aeson (encode, eitherDecode)
import Data.Traversable (for)
import Control.Monad (when)
import Data.Either (isLeft)
import Database.Transaction
import Control.Lens
import Data.Tuple.Extended (app3, app4)

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
           transactionM hasql $ statement User.Survey.insertAppPhoneCall $ app4 (const User.Survey.Hangup) (encodeHangup hangup)
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: hangup received " <> show hangup
         AnsweredWrapper _ -> undefined
         RecordWrapper record -> do
           hasql <- fmap (^. katipEnv . hasqlDbPool) ask
           transactionM hasql $ statement User.Survey.updateAppPhoneCall $ app3 (const User.Survey.Recorded) (encodeRecord record)
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: record received " <> show record
  when (isLeft res) $ $(logTM) ErrorS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: parse error: " <> show res