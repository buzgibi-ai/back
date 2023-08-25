{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Buzgibi.Api.Controller.Auth.Password.MakeLink (controller, ResetPasswordLink) where

import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Response (Response (Ok))
import Buzgibi.EnvKeys (Sendgrid (..))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Control.Lens
import Database.Transaction
import Control.Concurrent.Lifted (fork)
import Data.Foldable (for_)
import Control.Monad (void, when)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Hash (mkHash512)
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import "sendgrid" OpenAPI.Common
import Data.Coerce (coerce)
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem)
import Data.Maybe (isNothing)
import Katip.Controller
import Control.Monad.IO.Class
import Data.Int (Int64)

newtype ResetPasswordLink = ResetPasswordLink Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)

instance ToSchema ResetPasswordLink

controller :: ResetPasswordLink -> KatipControllerM (Response (Maybe Int64))
controller (ResetPasswordLink email) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
  let hash = mkHash512 $ show tm <> unpack email
  res <- transactionM hasql $ statement Auth.insertPasswordResetLink (email, hash)
  when (isNothing res) $
    void $ fork $ do
      cfg <- fmap (^. katipEnv . sendGrid) ask
      for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
        let link = "https://buzgibi.app/#/auth/password/reset?key=" <> hash
        let reqBody = 
              mkPOSTMailSendRequestBody 
              [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" ("password reset link: " <> link)]
              ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
              [(mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
              { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "password reset"
              } ]
              "password reset"
        liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
  return $ Ok res