{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Api.Controller.Utils (extractMIMEandExts)
import Buzgibi.Api.Controller.Webhook.CatchBark (commitToMinio) 
import Buzgibi.Transport.Model.Telnyx
import Buzgibi.Api.CallApi
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Api.CallApi.Instance ()  
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
import Data.Tuple.Extended (app3, app4, del2, snocT)
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
import Data.Maybe (fromMaybe)

data UrlError = NetworkFailure B.ByteString | UserMissing
  deriving Show

type instance Api "calls/{call_control_id}/actions/record_start" RecordingStartRequest RecordingStartResponse = ()

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
           transactionM hasql $ statement User.Survey.insertAppPhoneCall $ app4 Just $ snocT User.Survey.Hangup (encodeHangup hangup)
           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: hangup received " <> show hangup
         AnsweredWrapper answered@Answered {..} -> do
           env <- fmap (^. katipEnv) ask
           telnyxApiCfg <- fmap (ApiCfg (fromMaybe undefined (env^.telnyx)) (env^.httpReqManager)) askLoggerIO
           let request = RecordingStartRequest { recordingStartRequestFormat = MP3,  recordingStartRequestChannels = Single }
           let queryParam = [("{call_control_id}", answeredCallControlId)]
           callRes <- liftIO $ callApi @("calls/{call_control_id}/actions/record_start") @RecordingStartRequest @RecordingStartResponse 
                         telnyxApiCfg (Left request) methodPost queryParam Left (const (Right ()))

           hasql <- fmap (^. katipEnv . hasqlDbPool) ask              
           res <- for callRes $ const $ transactionM hasql $ 
             statement User.Survey.insertAppPhoneCall $ 
               snocT User.Survey.Answered $ snocT Nothing $ del2 (encodeAnswered answered)

           when (isLeft res) $ $(logTM) ErrorS (logStr @String ("Buzgibi.Api.Controller.Webhook.CatchTelnyx --> answered case has failed, error: " <> show res))    

         RecordWrapper record@Record {recordConnectionId, recordCallLegId, recordRecordingUrls=Payload {..}} -> do
           
           let getRecordingUrls = (decode . encode) =<< K.lookup "mp3" getPayload <|> K.lookup "wav" getPayload
           let status = maybe User.Survey.UrlLinkBroken (const User.Survey.Recorded) getRecordingUrls

           $(logTM) InfoS $ logStr $ "Buzgibi.Api.Controller.Webhook.CatchTelnyx: url ---> " <> show getRecordingUrls

           hasql <- fmap (^. katipEnv . hasqlDbPool) ask

           for_ getRecordingUrls $ \url -> do
             res <- E.runExceptT $ do
               manager <- lift $ fmap (^. katipEnv . httpReqManager) ask
               file_resp <- liftIO $ Request.make url manager [] HTTP.methodGet (Left (Nothing @()))
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