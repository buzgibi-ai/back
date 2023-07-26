{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Katip.Scribes.Minio (mkScribe) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import Hash (mkHash)
import Katip
import Network.Minio
import System.Directory
import System.FilePath
import Network.Mime (defaultMimeLookup)
import Data.String.Conv

mkScribe :: MinioConn -> Text -> PermitFunc -> Verbosity -> IO Scribe
mkScribe conn bucket permitF verb = do
  lock <- newMVar ()
  let logger conn lock item = 
        withMVar lock $ const $
          runMinioWith conn $ do
            (hash, path) <- liftIO $ do
              let hash = mkHash (item ^. itemTime)
              tmp <- getTemporaryDirectory
              let path = tmp </> unpack hash </> ".jaon"
              writeFile path $ 
                encodePretty (itemJson verb item)
                ^.from textbsl.from stext
              return (hash, path)
            exists <- bucketExists bucket
            unless exists $ makeBucket bucket Nothing
            fPutObject bucket hash path
              defaultPutObjectOptions
              { pooContentType = 
                Just (toS (defaultMimeLookup ".json")) }
  let finalize = return ()
  return $ Scribe (void . logger conn lock) finalize permitF
