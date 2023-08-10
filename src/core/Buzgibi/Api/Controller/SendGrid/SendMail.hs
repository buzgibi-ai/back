{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.SendGrid.SendMail (controller, SendGridSendMailRequest) where

import Buzgibi.Config (Email (..))
import Buzgibi.EnvKeys (Sendgrid (..), personEmail)
import Buzgibi.Transport.Response
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.Functor (($>))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Traversable (for)
import Data.Typeable (typeRep)
import GHC.Exts
import GHC.Generics
import Katip
import Katip.Controller
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types.Status (accepted202, ok200)
import "sendgrid" OpenAPI.Common
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem)

data SendGridSendMailRequest = SendGridSendMailRequest
  { from :: !Email,
    personalization :: !T.Text,
    subject :: !T.Text,
    body :: !T.Text
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor SendGridSendMailRequest)]]
          SendGridSendMailRequest

deriveToSchemaFieldLabelModifier
  ''SendGridSendMailRequest
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @SendGridSendMailRequest))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: SendGridSendMailRequest -> KatipControllerM (Response ())
controller req@SendGridSendMailRequest {..} = do
  $(logTM) InfoS $ logStr (show req)
  cfg <- fmap (^. katipEnv . sendGrid) ask
  resp <- for cfg $ \(Sendgrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    let reqBody =
          mkPOSTMailSendRequestBody
            [ mkPOSTMailSendRequestBodyContentsendgrid
                "text/plain"
                ( "from:"
                    <> coerce from
                    <> ", "
                    <> personalization
                    <> ", message: "
                    <> body
                )
            ]
            ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
            [ ( ( mkPOSTMailSendRequestBodyPersonalizationssendgrid
                    (map (mkToEmailArrayItem . coerce . personEmail) sendgridPersons)
                )
                  { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                    pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ subject
                  }
              )
            ]
            subject
    $(logTM) InfoS $ logStr (show (encodePretty reqBody))
    resp <- liftIO $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
    let handleResp resp =
          if responseStatus resp == ok200
            || responseStatus resp == accepted202
            then return $ Ok ()
            else
              $(logTM) ErrorS (logStr ("SendGrid error: " <> show (responseBody resp)))
                $> Error (asError @T.Text "something went wrong")
    handleResp resp
  when (isNothing resp) $ $(logTM) InfoS "sendgrid key hasn't been found. skip"
  return $ fromMaybe (Ok ()) resp
