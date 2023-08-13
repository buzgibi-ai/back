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


withWS 
  :: forall a . 
  FromJSON a =>
  WS.Connection -> 
  (a -> KatipControllerM ()) -> 
  KatipControllerM ()
withWS conn go =
  flip ST.evalStateT Nothing $ forever $ do
    res <- 
      fmap (eitherDecode @a) $ 
        liftIO $ 
          WS.receiveData @BSL.ByteString conn
    aesonRes <- for res $ \_data -> do
      new <- lift $ Async.async $ go _data
      old <- ST.state $ \old -> (old, Just new)
      for_ old $ lift . Async.cancel
    
    whenLeft aesonRes $ \error -> $(logTM) ErrorS $ logStr $ " ws aeson parse error ---> " <> error