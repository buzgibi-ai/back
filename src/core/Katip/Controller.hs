{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Katip.Controller
  ( Config (..),
    KatipControllerM (..),
    KatipEnv (..),
    KatipLogger (..),
    KatipState (..),
    KatipLoggerIO,
    KatipLoggerLocIO,
    KatipControllerState (..),
    Minio (..),
    defKatipEnv,

    -- * lens
    nm,
    ctx,
    env,
    katipEnv,
    terminal,
    httpReqManager,
    apiKeys,
    minio,
    bucketPrefix,
    hasqlDbPool,
    conn,
    telegram,
    captchaKey,
    jwk,
    github,
    bark,

    -- * run
    runKatipController,

    -- * re-export
    module R,

    -- * katip
    askLoggerIO,

    -- * aux
    runTelegram,
    sendGrid,
  )
where

import Async.Telegram
import Buzgibi.Config (SendGrid)
import Buzgibi.EnvKeys
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader.Class as R
import Control.Monad.Time
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.JWT as Jose
import Data.Default.Class
import Data.Monoid.Colorful (Term)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import "time" Data.Time
import Data.Typeable
import qualified Hasql.Connection as Hasql
import Katip
import Katip.Monadic
import Language.Haskell.TH.Syntax
import Network.HTTP.Client
import qualified Network.Minio as Minio
import "sendgrid" OpenAPI.Common as SendGrid
import Pretty
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError

type KatipLoggerIO = Severity -> LogStr -> IO ()

type KatipLoggerLocIO = Maybe Loc -> Severity -> LogStr -> IO ()

data KatipEnv = KatipEnv
  { katipEnvTerminal :: !Term,
    katipEnvHasqlDbPool :: !(Pool.Pool Hasql.Connection),
    katipEnvHttpReqManager :: !Manager,
    katipEnvApiKeys :: ![(String, String)],
    katipEnvMinio :: !Minio,
    katipEnvTelegram :: !Async.Telegram.Service,
    katipEnvSendGrid :: !(Maybe (SendGrid, SendGrid.Configuration)),
    katipEnvCaptchaKey :: !(Maybe T.Text),
    katipEnvJwk :: !Jose.JWK,
    katipEnvGithub :: !(Maybe Github),
    katipEnvBark :: !(Maybe Bark)
  }

defKatipEnv :: KatipEnv
defKatipEnv = 
      KatipEnv
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined 
      undefined

data Minio = Minio {minioConn :: !Minio.MinioConn, minioBucketPrefix :: !T.Text}

newtype KatipLogger = KatipWriter [String]
  deriving newtype (Monoid)
  deriving newtype (Semigroup)
  deriving newtype (NFData)

newtype KatipState = KatipState Int
  deriving newtype (Default)
  deriving newtype (NFData)

data Config = Config
  { configNm :: !Namespace,
    configCtx :: !LogContexts,
    configEnv :: !LogEnv,
    configKatipEnv :: !KatipEnv
  }

instance MonadTime Handler where
  currentTime = liftIO getCurrentTime

data KatipControllerState = KatipControllerState Int (TChan TelegramMsg)

newtype KatipControllerWriter = KatipControllerWriter [String]
  deriving newtype (Monoid)
  deriving newtype (Semigroup)

-- ServerM
newtype KatipControllerM a = KatipControllerM
  { unwrap ::
      RWS.RWST
        Config
        KatipControllerWriter
        KatipControllerState
        Handler
        a
  }
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadIO)
  deriving newtype (MonadReader Config)
  deriving newtype (MonadState KatipControllerState)
  deriving newtype (MonadWriter KatipControllerWriter)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadError ServerError)
  deriving newtype (MonadCatch)
  deriving newtype (MonadThrow)
  deriving newtype (MonadMask)
  deriving newtype (MonadTime)
  deriving newtype (MonadRWS Config KatipControllerWriter KatipControllerState)

makeFields ''Config
makeFields ''KatipEnv
makeFields ''Minio

-- These instances get even easier with lenses!
instance Katip KatipControllerM where
  getLogEnv = KatipControllerM $ asks configEnv
  localLogEnv f (KatipControllerM m) = KatipControllerM (local (over env f) m)

instance KatipContext KatipControllerM where
  getKatipContext = KatipControllerM $ asks configCtx
  localKatipContext f (KatipControllerM m) = KatipControllerM (local (over ctx f) m)
  getKatipNamespace = KatipControllerM $ asks configNm
  localKatipNamespace f (KatipControllerM m) = KatipControllerM (local (over nm f) m)

runKatipController :: Config -> KatipControllerState -> KatipControllerM a -> Handler a
runKatipController cfg st app = fmap fst (RWS.evalRWST (unwrap app) cfg st)

runTelegram :: Typeable a => String -> a -> KatipControllerM ()
runTelegram location msg = do
  KatipControllerState _ ch <- get
  liftIO $ atomically $ ch `writeTChan` TelegramMsg (mkPretty "At module " location) msg
