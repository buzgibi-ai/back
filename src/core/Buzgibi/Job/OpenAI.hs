{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Job.OpenAI (getTranscription, OpenAIEnv (..)) where


import Buzgibi.Statement.User.Survey (getSurveysForTranscription, insertTranscription, OpenAIPhone (..))
import qualified Network.HTTP.Client as HTTP
import qualified Hasql.Connection as Hasql
import Data.Pool (Pool)
import Katip
import Buzgibi.EnvKeys (OpenAI (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Database.Transaction
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import Data.Aeson (eitherDecode, encode)
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Control.Concurrent.Async as Async

data OpenAIEnv =
     OpenAIEnv 
     { logger :: Severity -> LogStr -> IO (), 
       pool :: Pool Hasql.Connection,
       openaiCfg :: OpenAI,
       manager :: HTTP.Manager
     }

getTranscription :: OpenAIEnv -> IO ()
getTranscription OpenAIEnv {..} = forever $ do 
  threadDelay (5 * 10 ^ 6)
  start <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.OpenAI: start at " <> show start
  xs <- transaction pool logger $ statement getSurveysForTranscription ()
  Async.forConcurrently_ xs $ \(survIdent, ys) -> do 
    let phones = sequence $ map (eitherDecode @OpenAIPhone . encode) ys
    res <- for phones $ \xs -> do
      ys <- Async.forConcurrently xs $ 
        \OpenAIPhone {..} -> undefined
      transaction pool logger $ statement insertTranscription (survIdent, ys)
    when (isLeft res) $ logger ErrorS $ logStr $ "Buzgibi.Job.OpenAI: phone parse failed for survey " <> show survIdent

  end <- getCurrentTime
  logger InfoS $ logStr $ "Buzgibi.Job.OpenAI: end at " <> show end