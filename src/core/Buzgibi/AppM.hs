{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Buzgibi.AppM (AppM (..)) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict as RWS
import Control.Monad.Trans.Control
import Katip.Controller
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))

newtype AppM a = AppM {runAppMonad :: RWS.RWST KatipEnv KatipLogger KatipState IO a}
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadIO)
  deriving newtype (MonadReader KatipEnv)
  deriving newtype (MonadState KatipState)
  deriving newtype (MonadWriter KatipLogger)
  deriving newtype (MonadRWS KatipEnv KatipLogger KatipState)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadCatch)
  deriving newtype (MonadThrow)
  deriving newtype (MonadUnliftIO)

instance MonadUnliftIO (RWS.RWST KatipEnv KatipLogger KatipState IO) where
  withRunInIO inner = 
    RWS.RWST $ \r s -> do
      x <- withRunInIO $ \run -> 
        inner $ \m ->
          fmap fst $ RWS.evalRWST m r s
      pure (x, s, mempty) 