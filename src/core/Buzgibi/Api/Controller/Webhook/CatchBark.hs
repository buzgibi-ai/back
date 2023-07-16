{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Buzgibi.Api.Controller.Webhook.CatchBark (controller) where

import Buzgibi.Api.Controller.Utils (extractMIMEandExts)
import Buzgibi.Transport.Response (toEither)
import Buzgibi.Transport.Id (Id (..))
import qualified Buzgibi.Api.Controller.File.Upload as File.Upload
import qualified Buzgibi.Transport.Model.Bark as Bark
import Buzgibi.Transport.Payload (Payload (..))
import qualified Buzgibi.Statement.User.Enquiry as Enquiry 
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
import Data.Either.Combinators (maybeToRight)
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Control.Monad.Trans.Except as E
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Hash (mkHash)
import Servant.Multipart.File
import Control.Lens.Iso.Extended (textbs)
import Data.Bifunctor (first)

data Error = 
     OutputIsMissing | 
     AudioOutIsMissing | 
     NetworkFailure B.ByteString |
     MinioError String
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
        transactionM hasql $ statement Enquiry.updateBark (Bark.responseIdent resp, Enquiry.BarkStart)
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
           file_id <- commitToMinio file mime exts $ Bark.responseIdent resp
           minio_res <- for file_id $ \[ident] -> do 
             lift $ transactionM hasql $ statement Enquiry.insertVoice (Bark.responseIdent resp, Enquiry.BarkProcessed, coerce ident, Enquiry.ProcessedByBark)
           E.except minio_res
        when (isLeft res) $ $(logTM) ErrorS (logStr @String ("catch bark webhook --> file hasn't been saved, error: " <> show res))     
      _ -> $(logTM) InfoS (logStr @String ("catch bark webhook --> " <> show resp))


commitToMinio (file, _) mime extXs name
  | mime == "audio/wav" || 
    mime == "audio/x-wav" = do
      tmp <- liftIO getTemporaryDirectory
      let filePath = tmp </> T.unpack (mkHash file)
      liftIO $ B.writeFile filePath file
      lift $ fmap (first (MinioError . show) . join . Right . toEither) $ File.Upload.controller "bark" $ Files [File name (mime^.from textbs) filePath extXs]