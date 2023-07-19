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

module Katip.Scribes.Telegram (mkScribe) where

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

data Body = Body { chat_id :: String, text :: String, parse_mode :: String }
  deriving stock (Generic)
  deriving ToJSON

mkScribe :: HTTP.Manager -> Telegram -> PermitFunc -> Verbosity -> IO Scribe
mkScribe manager Telegram {..} permitF verbosity = do
  let finalize = return ()
  let bot = fromMaybe undefined telegramBot
  let url = telegramHost <> bot <> "/sendMessage"
  let logger item = do
        let msg = encodePretty (itemJson verbosity item) ^. from textbsl . from stext
        when (telegramEnv == Dev) $ do
          for_ ((reverse . split []) (toS msg)) $ \chunk -> do 
            let body = 
                  Body
                  { chat_id = toS telegramChat, 
                    text = "`" <> toS chunk <> "`",
                    parse_mode = "markdown" 
                  }
            Request.make url manager mempty HTTP.methodPost $ Just body
  return $ Scribe logger finalize permitF
  where 
    split xs source | BL.length source < 4096 = source : xs
    split xs old = let (x, new) = BL.splitAt 4096 old in split (x : xs) new