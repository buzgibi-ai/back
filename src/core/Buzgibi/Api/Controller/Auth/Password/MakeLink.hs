{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module Buzgibi.Api.Controller.Auth.Password.MakeLink (controller, ResetPasswordLink) where

import Buzgibi.Api.Controller.Utils (withError)
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Response (Response)
import Buzgibi.EnvKeys (Sendgrid (..))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Swagger (ToSchema)
import Control.Lens
import Database.Transaction
import Control.Concurrent.Lifted (fork)
import Data.Foldable (for_)
import Control.Monad (void)
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
import Katip.Controller
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia

newtype ResetPasswordLink = ResetPasswordLink Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)

instance ToSchema ResetPasswordLink

data InsertionResult = Success | TMLeft Int64 | User404
    deriving stock (Generic)
    deriving
    (FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal, ConstructorTagModifier '[CamelTo2 "_"]]
        InsertionResult

controller :: ResetPasswordLink -> KatipControllerM (Response (Maybe Int64))
controller (ResetPasswordLink email) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
  let hash = mkHash512 $ show tm <> unpack email
  res <- 
    fmap (eitherDecode @InsertionResult . encode) $ 
      transactionM hasql $ 
        statement Auth.insertPasswordResetLink (email, hash)
  for_ res $ \case
    Success -> do
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
    _ -> return ()       
  return $ withError res \case (TMLeft sec) -> Just sec; _ -> Nothing