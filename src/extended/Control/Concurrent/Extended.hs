module Control.Concurrent.Extended (module Concurrent, throttle) where

import Control.Concurrent as Concurrent
import Control.Concurrent.MSemN
import Control.Exception
import Control.Monad
import qualified Control.Concurrent.Async as Async

-- | Limit the number of @tasks@ started per second. @throttle@ will run all
--   actions concurrently but only starting a certain number per second. It
--   will wait for all tasks and return the results in a list.
throttle ::
  -- | number of tasks per second (TPS)
  Int ->
  -- | the tasks to run concurrently but limited by TPS
  [IO a] ->
  -- | the tasks results
  IO [Either SomeException a]
throttle tps tasks = do
  sem <- new tps
  let runTask task = wait sem 1 >> (Async.async task >>= Async.waitCatch)
  let timeResetWorker = 
        forever $ do 
          threadDelay 1000000
          signalF sem (\i -> (tps - i, ()))
  let runAllTasks = mapM runTask tasks
  bracket (forkIO timeResetWorker) killThread (const runAllTasks)