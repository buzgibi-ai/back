{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Api.Controller.WS.History (controller) where

import Buzgibi.Api.Controller.WS.WithWS (withWS)
import Buzgibi.Transport.Response (Response(Ok))
import Buzgibi.Auth (AuthenticatedUser (..))
import Katip.Controller
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, encode)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Control.Concurrent (threadDelay)

newtype Page = Page Int64
  deriving newtype (FromJSON)

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller user conn = withWS user conn resolveBarkVoice

resolveBarkVoice :: WS.Connection -> AuthenticatedUser -> Int64 -> KatipControllerM ()
resolveBarkVoice conn _ page = do
  liftIO $ threadDelay $ 10 ^ 6
  let resp = Ok [show page]
  liftIO $ WS.sendDataMessage conn (WS.Text (encode resp) Nothing)