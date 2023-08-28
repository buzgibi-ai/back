{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.Auth.Register (controller) where

import Buzgibi.EnvKeys (Sendgrid (..))
import Buzgibi.Api.Controller.Utils (withError)
import qualified Buzgibi.Auth as Auth
import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Model.User (AuthToken (..), Credentials (email, password))
import Buzgibi.Transport.Response (Response)
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad (join, void, when)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Traversable (for)
import Database.Transaction
import Katip.Controller
import Control.Concurrent.Lifted (fork)
import qualified Data.Text as T
import Data.Foldable (for_)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Hash (mkHash)
import Data.Int (Int64)
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
import qualified Data.Hashable as H (hash)

data Error = UserTaken | JWT

instance Show Error where
  show UserTaken = "user is taken"
  show JWT = "jwt generation failed"

controller :: Credentials -> KatipControllerM (Response AuthToken)
controller cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident = liftIO . Auth.generateJWT key ident
  res <- fmap join $ transactionM hasql $ do
    identm <- statement Auth.insertUser (email cred, password cred)
    fmap (maybeToRight UserTaken) $ for identm $ \ident -> do
      tokene <- mkToken ident $ email cred
      fmap (first (const JWT)) $
        for tokene $ \(tokenbs, uuid) -> do
          let token = tokenbs ^. bytesLazy . from textbs
          let uuid_hash = fromIntegral @_ @Int64 $ H.hash uuid
          _ <- statement Auth.insertJwt (ident, token, uuid_hash)
          return (ident, token)
  for_ res $ \(ident, _) -> void $ fork $ sendConfirmationLink ident $ email cred         
  return $ withError res $ AuthToken . snd

sendConfirmationLink :: Int64 -> T.Text -> KatipControllerM ()
sendConfirmationLink ident email = do 
  tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
  let hash = mkHash $ show tm <> T.unpack email
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  ok <- transactionM hasql $ statement Auth.insertConfirmationLink (ident, hash)
  when ok $ do 
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