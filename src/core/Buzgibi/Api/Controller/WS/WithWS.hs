{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Buzgibi.Api.Controller.WS.WithWS (withWS) where


import Katip
import Katip.Controller
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, eitherDecode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Control.Monad.State.Strict as ST
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Trans.Class (lift)
import Data.Either.Combinators (whenLeft)
import Control.Exception.Lifted (onException)
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Control.Lens


withWS 
  :: forall a . 
  (FromJSON a, Show a) =>
  WS.Connection -> 
  (Hasql.Connection -> a -> KatipControllerM ()) -> 
  KatipControllerM ()
withWS conn go = do
  $(logTM) DebugS $ logStr $ " ws connection established"
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  (db, local) <- liftIO $ Pool.takeResource hasql
  let onError = do
        liftIO $ Pool.putResource local db
        lift $ $(logTM) ErrorS $ logStr $ " ws closed, resource has been release"
  flip ST.evalStateT Nothing $ 
      flip onException onError $
        forever $ do
          res <- fmap (eitherDecode @a) $ liftIO $ WS.receiveData @BSL.ByteString conn
          lift $ $(logTM) DebugS $ logStr $ " ws reload, req: " <> show res
          aesonRes <- for res $ \_data -> do
            new <- lift $ Async.async $ go db _data
            old <- ST.state $ \old -> (old, Just new)
            for_ old $ \a -> do
               lift $ $(logTM) DebugS $ logStr $ "async canceled"
               lift $ Async.cancel a
          whenLeft aesonRes $ \error -> $(logTM) ErrorS $ logStr $ " ws aeson parse error ---> " <> error