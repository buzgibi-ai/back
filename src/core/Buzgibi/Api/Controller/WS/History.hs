{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Buzgibi.Api.Controller.WS.History (controller) where

import Buzgibi.Auth (AuthenticatedUser)
import Katip.Controller
import qualified Network.WebSockets as WS

controller :: AuthenticatedUser -> WS.Connection -> KatipControllerM ()
controller _ _ = undefined