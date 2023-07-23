{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Buzgibi.Api.Controller.User.MakeSurvey (controller, Survey) where

import qualified Buzgibi.Transport.Model.Bark as Bark
import qualified Buzgibi.Statement.User.Survey as Survey
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecodeStrict)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Transaction
import Katip.Controller
import Control.Lens
import Data.Default.Class
import Data.Either.Combinators (maybeToRight)
import Buzgibi.Api.Controller.Utils (withError)
import Data.Traversable (for)
import Data.Coerce (coerce)
import qualified Control.Concurrent.Lifted as Concurrent (fork)
import Data.Foldable (for_)
import Control.Monad (join)
import qualified Request as Request (make)
import qualified Network.HTTP.Types as HTTP
import Buzgibi.EnvKeys (url, version, key)
import Control.Monad.IO.Class (liftIO)
import Control.Lens.Iso.Extended (textbs)
import Katip
import Data.Int (Int64)
import Control.Monad (void)
import qualified Network.Minio as Minio
import Data.Time.Clock (getCurrentTime)
import qualified Conduit as Conduit
import qualified Data.ByteString as B

data Error = BarkCredentials404 | InsertionFail

instance Show Error where
  show BarkCredentials404 = "we cannot perform the request"
  show InsertionFail = "new enquiry entry cannot be fulfilled"

data Location = Location
  { locationLatitude :: Double,
    locationLongitude :: Double
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Location)]]
          Location

data Survey = Survey
  { surveySurvey :: !T.Text,
    surveyLocation :: Location,
    surveyCategory :: Survey.Category,
    surveyAssessmentScore :: Survey.AssessmentScore,
    surveyPhonesFileIdent :: [Int64]
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Survey)]]
          Survey

deriveToSchemaFieldLabelModifier ''Location [|modify (Proxy @Location)|]
deriveToSchemaFieldLabelModifier ''Survey [|modify (Proxy @Survey)|]

controller :: AuthenticatedUser -> Survey -> KatipControllerM (Response ())
controller _ Survey {surveySurvey} | T.length surveySurvey == 0 = return $ Error $ asError @T.Text "empty survey"
controller user survey@Survey {surveySurvey, surveyCategory, surveyAssessmentScore, surveyPhonesFileIdent,  surveyLocation = Location {..}} = do
  $(logTM) DebugS (logStr ("survey ---> " <> show survey))
  barkm <- fmap (^. katipEnv . bark) ask
  manager <- fmap (^. katipEnv . httpReqManager) ask
  resp <- fmap (join .  maybeToRight BarkCredentials404) $ 
    for barkm $ \bark -> do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      let survey = 
            def { 
              Survey.surveyUserId = coerce user,
              Survey.surveySurvey = surveySurvey,
              Survey.surveyStatus = Survey.Received,
              Survey.surveyLatitude = locationLatitude,
              Survey.surveyLongitude = locationLongitude,
              Survey.surveyCategory = surveyCategory,
              Survey.surveyAssessmentScore = surveyAssessmentScore,
              Survey.surveyPhones = head surveyPhonesFileIdent
            }
      identm <- transactionM hasql $ statement Survey.insert survey
      for_ identm $ \survey_ident -> 
        Concurrent.fork $ do

          Minio {..} <- fmap (^. katipEnv . minio) ask
          -- parse file and assign phones to survey
          void $ assignPhonesToSurvey hasql minioConn survey_ident

          resp <- liftIO $ 
            Request.make
              (bark^.url) manager 
              [(HTTP.hAuthorization, "Token " <> (bark^.key.textbs))] 
              HTTP.methodPost $ 
              Left (Just (mkReq (bark^.version) surveySurvey))
          let mkBark ident st = 
                Survey.Bark {
                  Survey.barkReq = toJSON $ mkReq (bark^.version) surveySurvey,
                  Survey.barkStatus = st,
                  Survey.barkIdent = ident,
                  Survey.barkSurveyId = survey_ident
                }
          case resp of
            Right (resp, _) -> do 
               let bark_resp = eitherDecodeStrict @Bark.Response resp
               case bark_resp of 
                 Right resp -> 
                   transactionM hasql $ 
                     statement Survey.insertBark $
                       mkBark (Bark.responseIdent resp) Survey.BarkSent
                 Left err -> $(logTM) ErrorS (logStr ("bark response resulted in error: " <> show err))
            Left err -> $(logTM) ErrorS (logStr ("bark response resulted in error: " <> show err))
      return $ maybeToRight InsertionFail identm
  return $ withError resp $ const ()

data Input = Input { inputPrompt :: T.Text }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Input)]]
          Input

data BarkRequestBody =
     BarkRequestBody
     { barkRequestBodyVersion :: T.Text,
       barkRequestBodyInput :: Input,
       barkRequestBodyWebhook :: T.Text,
       barkRequestBodyWebhook_events_filter :: [T.Text] 
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor BarkRequestBody)]]
          BarkRequestBody

-- {
--   https://replicate.com/suno-ai/bark/api
--   "version": "5c7d5dc6dd8bf75c1acaa8565735e7986bc5b66206b55cca93cb72c9bf15ccaa",
--   "input": {
--     "text": "Alice"
--   },
--   "webhook": "https://buzgibi.app/foreign/webhook/bark",
--   "webhook_events_filter": ["start", "completed"]
-- }
mkReq version survey = BarkRequestBody version (Input survey) "https://buzgibi.app/foreign/webhook/bark" ["start", "completed"]

assignPhonesToSurvey hasql minio surveyIdent = do 
  (bucket, hash) <- transactionM hasql $ statement Survey.getPhoneMeta surveyIdent
  tm <- show <$> liftIO getCurrentTime
  minioRes <- liftIO $ Minio.runMinioWith minio $ do 
    o <- Minio.getObject bucket hash Minio.defaultGetObjectOptions
    Conduit.runConduit $ do
      path <- Minio.gorObjectStream o
        Conduit..| Conduit.sinkSystemTempFile 
          (T.unpack hash)
      -- P.S. build a conduit pipeline    
      liftIO $ fmap (^.from textbs.to (T.splitOn "\n")) $ B.readFile path    
  case minioRes of 
    Right phoneXs -> transactionM hasql $ statement Survey.insertPhones (surveyIdent, phoneXs)
    Left err -> $(logTM) ErrorS (logStr (show err))