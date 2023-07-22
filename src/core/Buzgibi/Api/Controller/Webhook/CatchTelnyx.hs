{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Api.Controller.Utils (extractMIMEandExts)
import Buzgibi.Api.Controller.Webhook.CatchBark (commitToMinio) 
import Buzgibi.Transport.Model.Telnyx
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Id (Id (..))
import qualified Buzgibi.Statement.User.Survey as User.Survey (insertVoiceTelnyx, getUserByAppIdent, insertAppPhoneCall, updateAppPhoneCall, CallStatus (..))
import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))
import Data.Aeson (encode, eitherDecode, decode)
import Data.Traversable (for)
import Control.Monad (when)
import Data.Either (isLeft)
import Database.Transaction
import Control.Lens
import Data.Tuple.Extended (app3, app4)
import qualified Data.Aeson.KeyMap as K (lookup)
import Control.Applicative ((<|>))
import qualified Request as Request (make)
import Data.Foldable (for_)
import qualified Control.Monad.Trans.Except as E
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as B
import Data.Either.Combinators (maybeToRight)
import Data.Bifunctor (first)
import Data.Coerce (coerce)

data UrlError = NetworkFailure B.ByteString | UserMissing
  deriving Show

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
         RecordWrapper record@Record {recordConnectionId, recordCallLegId, recordRecordingUrls=Payload {..}} -> do
           
           let getRecordingUrls = (decode . encode) =<< K.lookup "mp3" getPayload <|> K.lookup "wav" getPayload
           let status = maybe User.Survey.UrlLinkBroken (const User.Survey.Recorded) getRecordingUrls

           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: url ---> " <> show getRecordingUrls

           hasql <- fmap (^. katipEnv . hasqlDbPool) ask

           for_ getRecordingUrls $ \url -> do
             res <- E.runExceptT $ do
               manager <- lift $ fmap (^. katipEnv . httpReqManager) ask
               file_resp <- liftIO $ Request.make url manager [] HTTP.methodGet (Nothing @())
               file <- E.withExceptT NetworkFailure $ E.except file_resp
               let (mime, exts) = extractMIMEandExts url
               usere <- lift $ transactionM hasql $ statement User.Survey.getUserByAppIdent recordConnectionId
               (user, surveyTitle) <- fmap (first AuthenticatedUser) $ E.except $ maybeToRight UserMissing $ usere
              
               let fileTitle = surveyTitle <> "_" <> recordCallLegId
               file_id <- commitToMinio user file mime "telnyx" exts fileTitle
               for file_id $ \[ident] -> do
                 lift $ transactionM hasql $ do
                   statement User.Survey.insertVoiceTelnyx (recordConnectionId, recordCallLegId, coerce ident)
                   statement User.Survey.updateAppPhoneCall $ app3 (const status) (encodeRecord record)

             when (isLeft res) $ $(logTM) ErrorS (logStr @String ("Buzgibi.Api.Controller.Webhook.CatchTelnyx --> record case has failed, error: " <> show res))   
            
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: record received " <> show record
  when (isLeft res) $ $(logTM) ErrorS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: parse error: " <> show res