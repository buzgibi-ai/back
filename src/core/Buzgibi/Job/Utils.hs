module Buzgibi.Job.Utils (withElapsedTime) where

import Data.Time.Clock (getCurrentTime)
import Katip

withElapsedTime logger loc go = do 
  start <- getCurrentTime
  logger InfoS $ logStr $ loc <> ": ---> start at " <> show start
  go
  end <- getCurrentTime
  logger InfoS $ logStr $ loc <> ": end at " <> show end