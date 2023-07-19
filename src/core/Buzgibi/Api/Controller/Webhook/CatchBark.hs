{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Buzgibi.Api.Controller.Webhook.CatchBark (controller) where

import Buzgibi.Api.Controller.Utils (extractMIMEandExts)
import Buzgibi.Transport.Response (toEither)
import Buzgibi.Transport.Id (Id (..))
import Buzgibi.Auth (AuthenticatedUser (..))
import qualified Buzgibi.Api.Controller.File.Upload as File.Upload
import qualified Buzgibi.Transport.Model.Bark as Bark
import Buzgibi.Transport.Payload (Payload (..))
import qualified Buzgibi.Statement.User.Survey as Survey
import Katip.Controller
import Katip
import Data.Aeson (eitherDecode, encode, Object)
import Data.Coerce (coerce)
import Control.Lens
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad (when, join)
import Data.Either (isLeft)
import Database.Transaction
import qualified Request as Request (make)
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP
import Data.Either.Combinators (maybeToRight, fromLeft')
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Control.Monad.Trans.Except as E
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Hash (mkHash)
import Servant.Multipart.File
import Control.Lens.Iso.Extended (textbs)
import Data.Bifunctor (first, second)
import qualified Network.Minio as Minio
import Control.Monad.Error.Class (throwError)
import Hasql.Session (QueryError (..), CommandError (..))
import Data.String (fromString)

data Error = 
     OutputIsMissing | 
     AudioOutIsMissing | 
     NetworkFailure B.ByteString |
     MinioError String |
     UserMissing
  deriving Show

controller :: Payload -> KatipControllerM ()
controller payload = do
  $(logTM) DebugS (logStr @String ("catch bark webhook, payload ---> " <> show payload))
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let respe = eitherDecode @Bark.Response $ encode @Object $ coerce payload
  when (isLeft respe) $ $(logTM) ErrorS (logStr @String ("catch bark webhook, error while parsing payload " <> show respe))
  for_ respe $ \resp ->
    case Bark.responseStatus resp of
      Bark.Starting -> do 
        $(logTM) DebugS (logStr @String ("catch bark webhook --> processing"))
        transactionM hasql $ statement Survey.updateBark (Bark.responseIdent resp, Survey.BarkStart)
      Bark.Succeeded -> do
        $(logTM) DebugS (logStr @String  ("catch bark webhook --> succeeded"))
        res <- E.runExceptT $ do
           out <- E.except $ maybeToRight OutputIsMissing $ Bark.responseOutput resp
           url <- E.except $ maybeToRight AudioOutIsMissing $ M.lookup (T.pack "audio_out") out
           manager <- lift $ fmap (^. katipEnv . httpReqManager) ask
           $(logTM) DebugS (logStr @String  ("catch bark webhook --> url: " <> show url))
           file_resp <- liftIO $ Request.make url manager [] HTTP.methodGet (Nothing @())
           file <- E.withExceptT NetworkFailure $ E.except file_resp
           let (mime, exts) = extractMIMEandExts url
            
           usere <- lift $ transactionM hasql $ statement Survey.getUserByBarkIdent $ Bark.responseIdent resp
           user <- fmap AuthenticatedUser $ E.except $ maybeToRight UserMissing $ usere

           file_id <- commitToMinio user file mime exts $ Bark.responseIdent resp
           minio_res <- for file_id $ \[ident] -> do
             Minio {..} <- lift $ fmap (^. katipEnv . minio) ask 
             lift $ transactionM hasql $ do
               statement Survey.insertVoice (Bark.responseIdent resp, Survey.BarkProcessed, coerce ident, Survey.ProcessedByBark)
               res <- makeSharableLink minioConn $ Bark.responseIdent resp
               when (isLeft res) $ throwError $ QueryError mempty mempty $ ClientError (Just (fromLeft' res))
           E.except minio_res
        when (isLeft res) $ $(logTM) ErrorS (logStr @String ("catch bark webhook --> file hasn't been saved, error: " <> show res))     
      _ -> $(logTM) InfoS (logStr @String ("catch bark webhook --> " <> show resp))

commitToMinio user (file, _) mime extXs name
  | mime == "audio/wav" || 
    mime == "audio/x-wav" = do
      tmp <- liftIO getTemporaryDirectory
      let filePath = tmp </> T.unpack (mkHash file)
      liftIO $ B.writeFile filePath file
      lift $ fmap (first (MinioError . show) . join . Right . toEither) $ 
        File.Upload.controller user "bark" $ 
          Files [File name (mime^.from textbs) filePath extXs]

makeSharableLink minio barkIdent = do
  logger <- ask
  liftIO $ logger DebugS $ logStr @String  (" makeSharableLink ---> bark ident: " <> T.unpack barkIdent)
  objectm <- statement Survey.getVoiceObject barkIdent
  fmap (join . maybeToRight "getVoiceObject not found") $ 
    for objectm $ \meta@(object, bucket, ident) -> do
      liftIO $ logger DebugS $ logStr @String  (" makeSharableLink ---> meta: " <> show meta)
      urlm <- liftIO $ fmap (second (^.from textbs)) $
        Minio.runMinioWith minio $ do
         Minio.presignedGetObjectUrl bucket object (7 * 24 * 3600) mempty mempty
      fmap (first (fromString . show)) $ for urlm $ \url -> statement Survey.insertShareLink (ident, url)