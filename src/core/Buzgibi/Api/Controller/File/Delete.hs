{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.File.Delete (controller) where

import Buzgibi.Statement.File as File
import Buzgibi.Transport.Id
import Buzgibi.Transport.Response
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Bool
import Data.Coerce
import Data.Int
import Database.Transaction
import Katip
import Katip.Controller

controller :: Id "file" -> KatipControllerM (Response ())
controller id = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id) ^. stext <> "} not found"
  $(logTM) DebugS (logStr (show id))
  isOk <- transactionM hasql $ statement File.delete id
  return $ bool (Error (asError notFound)) (Ok ()) isOk
