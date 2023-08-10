{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Buzgibi.Api.Controller.Webhook.CatchTelnyx (controller) where

import Buzgibi.Api.Controller.Webhook.CatchBark (commitToMinio) 
import Buzgibi.Transport.Model.Telnyx
import Buzgibi.Api.CallApi
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Api.CallApi.Instance ()  
import Buzgibi.Transport.Id (Id (..))
import qualified Buzgibi.Statement.User.Survey as User.Survey
import Katip.Controller
import Katip
import Buzgibi.Transport.Payload (Payload (..))
import Data.Aeson (encode, eitherDecode, decode)
import Data.Traversable (for)
import Control.Monad (when, join, void)
import Data.Either (isLeft)
import Database.Transaction
import Control.Lens
import Data.Tuple.Extended (app1)
import qualified Data.Aeson.KeyMap as K (lookup)
import Control.Applicative ((<|>))
import qualified Request as Request (make)
import qualified Control.Monad.Trans.Except as E
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as B
import Data.Either.Combinators (maybeToRight, whenLeft)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import BuildInfo (location)
import Data.String.Conv
import Network.Mime (defaultMimeLookup)


data UrlError = NetworkFailure B.ByteString | UserMissing
  deriving Show

type instance Api "calls/{call_control_id}/actions/record_start" RecordingStartRequest RecordingStartResponse = ()
type instance Api "calls/{call_control_id}/actions/hangup" HangupRequest () = ()
type instance Api "calls/{call_control_id}/actions/playback_start" PlaybackStartRequest PlaybackStartResponse = ()

controller :: Payload -> KatipControllerM ()
controller payload@Payload {..} = do
  $(logTM) InfoS $ logStr $ $location <> " payload ----> " <> show payload
  let parseRes = do 
        Webhook {..} <- eitherDecode @Webhook $ encode getPayload
        case webhookEventType of 
          CallAnswered -> fmap AnsweredWrapper $ eitherDecode @Answered $ encode webhookPayload
          CallHangup -> fmap HangupWrapper $ eitherDecode @Hangup $ encode webhookPayload
          CallRecordingSaved -> fmap RecordWrapper $ eitherDecode @Record $ encode webhookPayload
          CallMachineDetectionEnded -> fmap MachineDetectionWrapper $ eitherDecode @MachineDetection $ encode webhookPayload
          _ -> Right $ Skip webhookEventType
  res :: Either String () 
    <- for parseRes $ \case
         Skip evType -> $(logTM) InfoS $ logStr $ $location <> " (skip case) ---->  webhook " <> show evType <> " was skipped"
         HangupWrapper hangup ->
           if hangupHangupCause hangup == NormalClearing 
           then $(logTM) InfoS $ logStr $ $location <> " (hangup case) ---> hangup received " <> show hangup <> ", normal clearing. skip"
           else do 
             hasql <- fmap (^. katipEnv . hasqlDbPool) ask
             let (telnyxIdent, leg, cause) = encodeHangup hangup
             transactionM hasql $ statement User.Survey.insertHangupCall (leg, (toS . show) cause)
             transactionM hasql $ statement User.Survey.checkAfterWebhook telnyxIdent
             $(logTM) DebugS $ logStr $ $location <> " (hangup case) ---> hangup received " <> show hangup
         AnsweredWrapper answered@Answered {..} -> do
           env <- fmap (^. katipEnv) ask
           hasql <- fmap (^. katipEnv . hasqlDbPool) ask
           telnyxApiCfg <- fmap (ApiCfg (fromMaybe undefined (env^.telnyx)) (env^.httpReqManager)) askLoggerIO

           let msg = $location <> " ---- > voice url not found for a call " <> answeredCallControlId
           url <- fmap (fromMaybe (error (toS msg))) $ transactionM hasql $ statement User.Survey.getVoiceLinkByCallLegId answeredCallLegId

           let record_request = Just $ RecordingStartRequest { recordingStartRequestFormat = MP3,  recordingStartRequestChannels = Single }
           let playback_request = Just $ PlaybackStartRequest { playbackStartRequestAudioUrl = url }
           let queryParam = [("{call_control_id}", answeredCallControlId)]

           playbackResp <- liftIO $ callApi @("calls/{call_control_id}/actions/playback_start") @PlaybackStartRequest @PlaybackStartResponse telnyxApiCfg (Left playback_request) methodPost queryParam Left (const (Right ()))
           recordResp <- liftIO $ callApi @("calls/{call_control_id}/actions/record_start") @RecordingStartRequest @RecordingStartResponse telnyxApiCfg (Left record_request) methodPost queryParam Left (const (Right ()))
          
           let resp = sequence [playbackResp, recordResp]

           res <- for resp $ const $ transactionM hasql $ 
             statement User.Survey.updateAppCall $ 
               app1 (const (toS (show User.Survey.Answered))) $ 
                 encodeAnswered answered

           when (isLeft resp) $ $(logTM) ErrorS (logStr @String ($location <> " (answer case) --> answered case has failed, error: " <> show res))    
         
         RecordWrapper record@Record {recordConnectionId, recordCallLegId, recordRecordingUrls=Payload {..}} -> do
           
           let getRecordingUrls = 
                 fmap ("mp3",) (join (fmap (decode . encode) (K.lookup "mp3" getPayload))) <|> 
                 fmap ("wav",) (join (fmap (decode . encode) (K.lookup "wav" getPayload)))

           $(logTM) InfoS $ logStr $ $location <> " (record case) url ---> " <> show getRecordingUrls

           hasql <- fmap (^. katipEnv . hasqlDbPool) ask

           res <- for (maybeToRight () getRecordingUrls) $ \(ext, url) -> do
             res <- E.runExceptT $ do
               manager <- lift $ fmap (^. katipEnv . httpReqManager) ask
               file_resp <- liftIO $ Request.make url manager [] HTTP.methodGet (Left (Nothing @()))
               file <- E.withExceptT NetworkFailure $ E.except file_resp
               let mime = defaultMimeLookup ".mp3"
               usere <- lift $ transactionM hasql $ statement User.Survey.getUserByAppIdent recordConnectionId
               (user, surveyTitle) <- fmap (first AuthenticatedUser) $ E.except $ maybeToRight UserMissing $ usere
              
               let fileTitle = surveyTitle <> "_" <> recordCallLegId
               file_id <- commitToMinio user file mime "telnyx" [ext] fileTitle
               for file_id $ \([ident], _) -> lift $ do
                  transactionM hasql $ statement User.Survey.insertVoiceTelnyx (recordCallLegId, coerce ident)
                  transactionM hasql $ statement User.Survey.checkAfterWebhook recordConnectionId
             when (isLeft res) $ $(logTM) ErrorS (logStr @String ($location <> " (record case) --> record case has failed, error: " <> show res))

           whenLeft res $ const $ $(logTM) ErrorS $ logStr $ $location <> " (record case) ---> url broken " <> show getPayload

           $(logTM) InfoS $ logStr $ $location <> " (record case) ---> record received " <> show record

         MachineDetectionWrapper MachineDetection {machineDetectionResult = Human} -> 
           $(logTM) InfoS $ logStr @String $ $location <> " (machine detection case) --> human. proceed with the call."
         MachineDetectionWrapper machine@MachineDetection {..} -> do
           $(logTM) InfoS $ logStr @String $ $location <> " (machine detection case) --> machine is detected." <> show machine
           hasql <- fmap (^. katipEnv . hasqlDbPool) ask
           transactionM hasql $ do 
             statement User.Survey.insertHangupCall (machineDetectionCallLegId, (toS . show) machineDetectionResult)
             statement User.Survey.checkAfterWebhook machineDetectionConnectionId
           env <- fmap (^. katipEnv) ask  
           telnyxApiCfg <- fmap (ApiCfg (fromMaybe undefined (env^.telnyx)) (env^.httpReqManager)) askLoggerIO  
           let request = Just $ HangupRequest machineDetectionClientState
           let queryParam = [("{call_control_id}", machineDetectionCallControlId)]
           void $ liftIO $ callApi @("calls/{call_control_id}/actions/hangup") @HangupRequest @() telnyxApiCfg (Left request) methodPost queryParam Left (const (Right ()))

  when (isLeft res) $ $(logTM) CriticalS $ logStr $ $location <> " ---> parse error: " <> show res