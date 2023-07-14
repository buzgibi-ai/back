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

module Buzgibi.Api.Controller.User.MakeEnquiry (controller, Enquiry) where

import qualified Buzgibi.Transport.Model.Bark as Bark
import qualified Buzgibi.Statement.User.Enquiry as Enquiry
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

data Enquiry = Enquiry
  { enquiryEnquiry :: !T.Text,
    enquiryLocation :: Location
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Enquiry)]]
          Enquiry

deriveToSchemaFieldLabelModifier ''Location [|modify (Proxy @Location)|]
deriveToSchemaFieldLabelModifier ''Enquiry [|modify (Proxy @Enquiry)|]

controller :: AuthenticatedUser -> Enquiry -> KatipControllerM (Response ())
controller user enquiry@Enquiry {enquiryEnquiry, enquiryLocation = Location {..}} = do
  $(logTM) DebugS (logStr ("enquiry ---> " <> show enquiry))
  barkm <- fmap (^. katipEnv . bark) ask
  manager <- fmap (^. katipEnv . httpReqManager) ask
  resp <- fmap (join .  maybeToRight BarkCredentials404) $ 
    for barkm $ \bark -> do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      let enq = 
            def { 
              Enquiry.enquiryUserId = coerce user,
              Enquiry.enquiryEnquiry = enquiryEnquiry,
              Enquiry.enquiryStatus = Enquiry.Received,
              Enquiry.enquiryLatitude = locationLatitude,
              Enquiry.enquiryLongitude = locationLongitude
            }
      identm <- transactionM hasql $ statement Enquiry.insert enq
      for_ identm $ \enq_ident -> 
        Concurrent.fork $ do 
          resp <- liftIO $ 
            Request.make
              (bark^.url) manager 
              [(HTTP.hAuthorization, "Token " <> (bark^.key.textbs))] 
              HTTP.methodPost $ 
              Just (mkReq (bark^.version) enquiryEnquiry)
          let mkBark ident st = 
                Enquiry.Bark {
                  Enquiry.barkReq = toJSON $ mkReq (bark^.version) enquiryEnquiry,
                  Enquiry.barkStatus = st,
                  Enquiry.barkIdent = ident,
                  Enquiry.barkEnquiryId = enq_ident
                }
          case resp of
            Right (resp, _) -> do 
               let bark_resp = eitherDecodeStrict @Bark.Response resp
               case bark_resp of 
                 Right resp -> 
                   transactionM hasql $ 
                     statement Enquiry.insertBark $
                       mkBark (Bark.responseIdent resp) Enquiry.BarkSent
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
--   "webhook": "https://api.buzgibi.app/foreign/webhook/bark",
--   "webhook_events_filter": ["start", "completed"]
-- }
mkReq version enquiry = BarkRequestBody version (Input enquiry) "https://buzgibi.app/foreign/webhook/bark" ["start", "completed"]
