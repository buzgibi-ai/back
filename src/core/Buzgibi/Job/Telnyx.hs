{-# LANGUAGE RecordWildCards #-}

module Buzgibi.Job.Telnyx (makeCall, TelnyxEnv (..)) where

import Buzgibi.Statement.User.Survey (getPhonesToTelnyx)
import Database.Transaction
import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import Data.Time.Clock (getCurrentTime)

data TelnyxEnv = 
     TelnyxEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection 
     }

makeCall :: TelnyxEnv -> IO ()
makeCall TelnyxEnv {..} = forever $ do 
  threadDelay (60 * 10 ^ 6)
  start <- getCurrentTime
  logger DebugS $ logStr $ "Buzgibi.Job.Telnyx: start at " <> show start
  _ <- transaction pool logger $ statement getPhonesToTelnyx ()
  end <- getCurrentTime
  logger DebugS $ logStr $ "Buzgibi.Job.Telnyx: end at " <> show end