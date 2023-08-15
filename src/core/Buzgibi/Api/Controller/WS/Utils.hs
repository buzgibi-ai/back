{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Buzgibi.Api.Controller.WS.Utils (withWS, listen, Listen) where


import Katip
import Katip.Controller
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
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
import Control.Concurrent.Lifted (fork, killThread)
import qualified Control.Concurrent.MVar.Lifted as Async
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import qualified Hasql.Notifications as Hasql
import Data.Proxy (Proxy (..))
import Control.Lens.Iso.Extended (bytesLazy)
import Buzgibi.Api.Controller.Utils (withError)
import Data.String.Conv (toS)
import Control.Concurrent (threadDelay)
import qualified Data.Text.Lazy as TL

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

  thread <- Async.newEmptyMVar

  -- the first thread is solely responsible for receiving messages from frontend 
  front <- Async.async $ forever $ do 
    msg <- liftIO (WS.receiveData @BSL.ByteString conn)
    atomically $ Async.writeTChan ch msg
   
  -- the second one is for bd
  let release = do
        threadm <- Async.tryTakeMVar thread
        for_ threadm killThread
        Pool.putResource local db
  back <- Async.async $
    flip onException
    (liftIO release) $ forever $ do
      msg <- atomically $ Async.readTChan ch
      for_ (eitherDecode @a msg) $ \val -> do
        threadm <- Async.tryTakeMVar thread
        for_ threadm killThread
        forkId <- fork $ go db val
        Async.putMVar thread forkId

  keepAlive <- Async.async $ liftIO $ forever $ threadDelay (10 * 10 ^ 6) >> WS.sendPing @TL.Text conn mempty  

  (_, res) <- Async.waitAnyCatchCancel [front, back, keepAlive]
  whenLeft res $ \error -> $(logTM) ErrorS $ logStr $ $location <> " ws ends up with an error ---> " <> show error

type family Listen (s :: Symbol) (b :: Type) :: Constraint

listen :: forall s a b . (KnownSymbol s, Listen s a, FromJSON a, ToJSON b) => WS.Connection -> Hasql.Connection -> (a -> b) -> IO ()
listen c db go = do
  let channel =  toS $ symbolVal (Proxy @s)
  let channelToListen = Hasql.toPgIdentifier channel
  Hasql.listen db channelToListen
  forever $
    flip Hasql.waitForNotifications db $ 
      \channel payload -> do 
        let resp = eitherDecode @a $ payload^.from bytesLazy
        WS.sendDataMessage c (WS.Text (encode (withError resp go)) Nothing)