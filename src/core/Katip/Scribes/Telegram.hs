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

module Katip.Scribes.Telegram (mkScribe, mkService) where

import Katip
import Buzgibi.Config (Env (..), Telegram (..))
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client as HTTP
import Control.Lens.Iso.Extended
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Lens
import Request as Request
import qualified Data.ByteString.Lazy as BL
import Data.String.Conv
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Network.HTTP.Types as HTTP
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data Service = Service { sendMsg :: forall a  . LogItem a => Verbosity -> Item a -> IO ()}

data Body = Body { chat_id :: String, text :: String, parse_mode :: String }
  deriving stock (Generic)
  deriving ToJSON

mkService :: HTTP.Manager -> Telegram -> IO Service
mkService manager Telegram {..} = do
  let bot = fromMaybe undefined telegramBot
  let url = telegramHost <> bot <> "/sendMessage"
  let send verbosity item = do 
        let msg = encodePretty (itemJson verbosity item) ^. from textbsl . from stext
        when (telegramEnv == Dev) $ do
          for_ (splitByteString (toS msg)) $ \chunk -> do 
            let body = 
                  Body
                  { chat_id = toS telegramChat, 
                    text = "`" <> toS chunk <> "`",
                    parse_mode = "markdown" 
                  }
            Request.make url manager mempty HTTP.methodPost $ Just body
  return Service { sendMsg = send }
  where
    splitByteString =
      let split xs source
            | BL.length source < 4096 = source : xs
          split xs old =
            let (x, new) = BL.splitAt 4096 old
             in split (x : xs) new
       in reverse . split []

mkScribe :: Service -> PermitFunc -> Verbosity -> IO Scribe
mkScribe Service {..} permitF verb = do
  let finalize = return ()
  return $ Scribe (sendMsg verb) finalize permitF