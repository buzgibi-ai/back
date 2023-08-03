{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Katip.Scribes.Minio (mkScribe) where

import Control.Monad
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import Hash (mkHash)
import Katip
import Network.Minio
import Network.Mime (defaultMimeLookup)
import Data.String.Conv
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMQueue
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import BuildInfo (location)
import Data.Either.Combinators (whenLeft)
import Data.Foldable (for_)
import Data.Conduit.Combinators (sourceLazy)
import Control.Concurrent (threadDelay)

mkScribe :: MinioConn -> Text -> PermitFunc -> Verbosity -> IO Scribe
mkScribe conn bucket permitF verbosity = do
  lock <- newEmptyMVar
  logsQueue <- newTMQueueIO

  let logger logsQueue item = do
        let msg = encodePretty (itemJson verbosity item)
        atomically $ logsQueue `writeTMQueue` msg

  worker <- async $ forever $ do
      threadDelay (10 ^ 6)
      msgm <- atomically $ readTMQueue logsQueue
      for_ msgm $ \msg -> do
        resp <- runMinioWith conn $ do
          let hash = mkHash msg
          exists <- bucketExists bucket
          unless exists $ makeBucket bucket Nothing
          let opts = 
               defaultPutObjectOptions
               { pooContentType = 
                 Just (toS (defaultMimeLookup ".json")) }
          putObject bucket (toS hash) (sourceLazy msg) Nothing opts
        whenLeft resp $ \e -> print $ $location <> " ---> failed to send a message " <> toS msg <> ", error " <> show e      

  let finalize = do
        lock `putMVar` () 
        withMVar lock $ const $ do
          atomically $ closeTMQueue logsQueue
          cancel worker
          print $ $location <> " ---> minio scribe has been terminated gracefully"
  return $ Scribe (logger logsQueue) finalize permitF
