{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Buzgibi.Api.Controller.WS.History (controller) where

import Buzgibi.Api.Controller.Utils (withError)
import Buzgibi.Api.Controller.WS.WithWS (withWS)
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Control.Exception (onException)
import qualified Hasql.Notifications as Hasql
import Control.Lens.Iso.Extended (bytesLazy)
import Control.Lens
import Control.Monad (forever)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)

newtype Page = Page Int64
  deriving newtype (FromJSON)

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller _ conn =
  withWS @Page conn $ \page -> do 
   hasql <- fmap (^. katipEnv . hasqlDbPool) ask
   liftIO $ withResource hasql $ resolveVoice conn page

withResource :: Pool.Pool Hasql.Connection -> (Hasql.Connection -> IO ()) -> IO ()
withResource pool go = do
  (db, local) <- Pool.takeResource pool
  go db `onException` Pool.putResource local db
  Pool.putResource local db

data Voice = Voice { voiceSurvey :: Int64, voiceVoice :: Int64 }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Voice)]]
          Voice

resolveVoice :: WS.Connection -> Page -> Hasql.Connection -> IO ()
resolveVoice c _ db = do 
  let channelToListen = Hasql.toPgIdentifier "voice"
  Hasql.listen db channelToListen
  forever $
    flip Hasql.waitForNotifications db $ 
      \channel payload -> do 
        let resp = eitherDecode @Voice $ payload^.from bytesLazy
        WS.sendDataMessage c (WS.Text (encode (withError resp id)) Nothing)