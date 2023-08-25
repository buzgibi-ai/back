{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module Buzgibi.Api.Controller.Auth.Email.SendLink (controller) where

import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.EnvKeys (Sendgrid (..))
import Buzgibi.Transport.Response (Response (Ok))
import Katip.Controller
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (ToJSON, FromJSON, decode, encode)
import Data.Text (Text)
import Control.Lens
import Database.Transaction
import Control.Concurrent.Lifted (fork)
import Data.Foldable (for_)
import Control.Monad (void, join)
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
import Control.Monad.IO.Class

data LinkRes = Email Text | NextAttemptIn Int
    deriving stock (Generic, Show)
    deriving
    (ToJSON, FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal]
        LinkRes

controller :: AuthenticatedUser -> KatipControllerM (Response (Maybe Int))
controller (AuthenticatedUser {ident}) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
  let hash = mkHash512 $ show tm <> show ident
  res <- fmap (join . fmap (decode @LinkRes . encode)) $ transactionM hasql $ statement Auth.resendLink (ident, hash)
  for_ res $ \case
    Email email ->
      void $ fork $ do
        cfg <- fmap (^. katipEnv . sendGrid) ask
        for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
          let link = "https://buzgibi.app/#/auth/email/confirm?key=" <> hash
          let reqBody = 
                mkPOSTMailSendRequestBody 
                [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" ("confirmation link: " <> link)]
                ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
                [(mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
                { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "email confirmation"
                } ]
                "email confirmation"
          liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
    _ -> pure ()
  pure $ case res of
    Just (Email _) -> Ok Nothing
    Just (NextAttemptIn sec) -> Ok $ Just sec
    Nothing -> Ok Nothing