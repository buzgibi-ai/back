{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Buzgibi.Api.Controller.WS.WithWS (withWS) where


import Katip
import Katip.Controller
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, eitherDecode)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Foldable (for_)
import Data.Either.Combinators (whenLeft)
import Control.Exception.Lifted (onException)
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Control.Lens
import BuildInfo (location)
import qualified Control.Concurrent.STM.TChan.Lifted as Async
import Control.Concurrent.STM.Lifted (atomically)
import Control.Monad (forever)

withWS 
  :: forall a . 
  FromJSON a =>
  WS.Connection -> 
  (Hasql.Connection -> a -> KatipControllerM ()) -> 
  KatipControllerM ()
withWS conn go = do
  $(logTM) DebugS $ logStr $ " ws connection established"
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  (db, local) <- liftIO $ Pool.takeResource hasql
   
  ch <- atomically Async.newTChan

  -- the first thread is solely responsible for receiving messages from frontend 
  front <- Async.async $ forever $ do 
    msg <- liftIO $ WS.receiveData @BSL.ByteString conn
    atomically $ Async.writeTChan ch msg
   
  -- the second one is for bd
  let release = Hasql.release db >> Pool.putResource local db
  back <- Async.async $
    flip onException
    (liftIO release) $ do 
      msg <- atomically $ Async.readTChan ch
      for_ (eitherDecode @a msg) $ go db
      
  (_, res) <- Async.waitAnyCatchCancel [front, back]
  whenLeft res $ \error -> $(logTM) ErrorS $ logStr $ $location <> " ws ends up with an error ---> " <> show error