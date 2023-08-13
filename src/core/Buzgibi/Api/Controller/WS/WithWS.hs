{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Buzgibi.Api.Controller.WS.WithWS (withWS) where

import Buzgibi.Auth (AuthenticatedUser)
import Katip.Controller
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, eitherDecode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Control.Monad.State.Strict as ST
import Data.Foldable (for_)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool

withWS 
  :: forall a . 
  FromJSON a => 
  AuthenticatedUser -> 
  WS.Connection -> 
  (Pool.Pool Hasql.Connection -> WS.Connection -> AuthenticatedUser -> a -> KatipControllerM ()) -> 
  KatipControllerM ()
withWS user conn runWS =
  flip ST.evalStateT Nothing $ forever $ do
    res <- fmap (eitherDecode @a) $ liftIO $ WS.receiveData @BSL.ByteString conn
    for_ res $ \_data -> do
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      new <- lift $ Async.async $ forever $ runWS hasql conn user _data
      old <- ST.state $ \old -> (old, Just new)
      for_ old $ lift . Async.cancel