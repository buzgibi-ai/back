{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Katip.Scribes.Telegram (mkScribe) where

import Katip
import Buzgibi.Config (Telegram (..))
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client as HTTP
import Control.Lens.Iso.Extended
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Lens
import Request as Request
import qualified Data.ByteString.Lazy as BL
import Data.String.Conv
import Data.Foldable (for_)
import qualified Network.HTTP.Types as HTTP
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMQueue
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import BuildInfo (location)
import Control.Monad (forever)
import Data.Either.Combinators (whenLeft)


data Body = 
     Body 
     { chat_id :: String
     , text :: String
     , parse_mode :: String 
     }
  deriving stock (Generic, Show)
  deriving ToJSON

mkScribe :: HTTP.Manager -> Telegram -> PermitFunc -> Verbosity -> IO Scribe
mkScribe manager Telegram {..} permitF verbosity = do
  let bot = fromMaybe undefined telegramBot
  let url = telegramHost <> bot <> "/sendMessage"
  let split xs source | BL.length source < 4096 = source : xs
      split xs old = let (x, new) = BL.splitAt 4096 old in split (x : xs) new
  let logger logsQueue item = do
        let msg = encodePretty (itemJson verbosity item) ^. from textbsl . from stext
        atomically $ logsQueue `writeTMQueue` msg

  lock <- newEmptyMVar
  logsQueue <- newTMQueueIO

  worker <- async $ forever $ do
      threadDelay (2 * 10 ^ 6)
      msgm <- atomically $ readTMQueue logsQueue
      for_ msgm $ \msg ->
        for_ ((reverse . split []) (toS msg)) $ \chunk -> do
          let body =
               Body
               { chat_id = toS telegramChat,
                 text = "`" <> toS chunk <> "`",
                 parse_mode = "markdown" 
               }
          let contTypeH = (HTTP.hContentType, "application/json")
          resp <- Request.make url manager [contTypeH] HTTP.methodPost $ Left $ Just body
          whenLeft resp $ \e -> print $ $location <> " ---> failed to send a message " <> toS msg <> ", error " <> show e

  let finalize = do
        lock `putMVar` () 
        withMVar lock $ const $ do
          atomically $ closeTMQueue logsQueue
          cancel worker
          print $ $location <> " ---> telegram scribe has been terminated gracefully"
           
  return $ Scribe (logger logsQueue) finalize permitF