{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Api.Controller.File.Upload (controller) where

import BuildInfo
import Buzgibi.Statement.File as File
import Buzgibi.Transport.Id
import Buzgibi.Transport.Response
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Either
import Data.Foldable
import qualified Data.Text as T
import Data.Time.Clock
import Data.Traversable
import Database.Transaction
import Hash
import Katip
import Katip.Controller
import Network.Minio
import Servant.Multipart.File
import System.Directory
import System.FilePath
import System.Timeout

controller :: T.Text -> Files -> KatipControllerM (Response [Id "file"])
controller bucket x = do
  runTelegram $location (bucket, x)
  $(logTM) DebugS (logStr (show (bucket, x)))
  Minio {..} <- fmap (^. katipEnv . minio) ask
  es <- for (coerce x) $ \file@File {..} -> do
    tm <- liftIO getCurrentTime
    let hash = mkHash (fileName <> fileMime <> (show tm ^. stext))
    tmp <- liftIO getTemporaryDirectory
    let new_file_path = tmp </> T.unpack (mkHash file)
    liftIO $ copyFile filePath new_file_path
    runTelegram $location file {filePath = new_file_path}
    minioResult <- liftIO $ timeout (5 * 10 ^ 6) $ runMinioWith minioConn $ do
      let newBucket = minioBucketPrefix <> "." <> bucket
      exist <- bucketExists newBucket
      unless exist $
        makeBucket
          (minioBucketPrefix <> "." <> bucket)
          Nothing
      fPutObject newBucket hash filePath defaultPutObjectOptions
    $(logTM) DebugS (logStr (show minioResult))
    let file =
             NewFile 
              { newFileHash = hash, 
                newFileName = fileName, 
                newFileMime = fileMime,
                newFileBucket = bucket,
                newFileExts = fileExts
              }
    return $ maybe (Left (MErrIO (userError "minio server didn't respond"))) (fmap (const file)) minioResult
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let (error_xs, success_xs) = partitionEithers es
  ids <- transactionM hasql $ statement File.save success_xs
  for_ error_xs $ runTelegram $location
  return $ case ids of
    [] -> Errors $ map (asError . (\e -> show e ^. stext)) error_xs
    _ -> Warnings ids (map (asError . (\e -> show e ^. stext)) error_xs)
