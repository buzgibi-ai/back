{-# LANGUAGE RecordWildCards #-}

module Buzgibi.Job.Survey (makeReport, SurveyCfg (..)) where

import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import qualified Network.Minio as Minio

data SurveyCfg =
     SurveyCfg 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       manager :: HTTP.Manager,
       minio :: Minio.MinioConn
     }

makeReport :: SurveyCfg -> IO ()
makeReport SurveyCfg {..} = forever $ do 
  threadDelay (300 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.Survey: start at " <> show start

  -- 1 fetch data from db 
  -- 2 prepare a file 
  -- 3 commit a file to minio

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.OpenAI: end at " <> show end