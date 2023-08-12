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

withWS 
  :: forall a . 
  FromJSON a => 
  AuthenticatedUser -> 
  WS.Connection -> 
  (WS.Connection -> AuthenticatedUser -> a -> KatipControllerM ()) -> 
  KatipControllerM ()
withWS user conn runWS =
  flip ST.evalStateT Nothing $ forever $ do
    res <- fmap (eitherDecode @a) $ liftIO $ WS.receiveData @BSL.ByteString conn
    for_ res $ \_data -> do
      new <- lift $ Async.async $ forever $ runWS conn user _data
      old <- ST.get
      ST.put $ Just new
      for_ old $ lift . Async.cancel