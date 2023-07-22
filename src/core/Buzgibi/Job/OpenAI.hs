module Buzgibi.Job.OpenAI (getTranscription, OpenAIEnv (..)) where

import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import Katip
import Buzgibi.EnvKeys (OpenAI (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

data OpenAIEnv =
     OpenAIEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       openaiCfg :: OpenAI,
       manager :: HTTP.Manager
     }

getTranscription :: OpenAIEnv -> IO ()
getTranscription _ = forever $ do 
  threadDelay (300 * 10 ^ 6)
  undefined