module Buzgibi.Job.Utils (withElapsedTime) where

import Data.Time.Clock (getCurrentTime)
import Katip

withElapsedTime :: (Severity -> LogStr -> IO ()) -> String -> IO () -> IO ()
withElapsedTime logger loc go = do 
  start <- getCurrentTime
  logger InfoS $ logStr $ loc <> ": ---> start at " <> show start
  go
  end <- getCurrentTime
  logger InfoS $ logStr $ loc <> ": end at " <> show end