{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Buzgibi.Config
  ( Config,
    Katip (..),
    Db,
    Service (..),
    ApiKeys (..),
    Swagger (..),
    Telegram (..),
    Env (..),
    Cors (..),
    ServerError (..),
    Email (..),
    StdoutFormat (..),
    db,
    pass,
    port,
    database,
    host,
    user,
    poolN,
    tm,
    hasql,
    resPerStripe,
    katip,

    -- * load config
    load,
    path,
    verbosity,
    severity,
    stdoutFormat,
    env,
    service,
    accessKey,
    secretKey,
    logBucket,
    minio,
    bucketPrefix,
    swagger,
    bot,
    chat,
    telegram,
    serverConnection,
    cors,
    origins,
    serverError,
    webhook,
    jobFrequency,

    -- * Iso
    isoEnv,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Aeson.TH.Extended
import Data.String.Conv
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import TH.Mk

data Db = Db
  { dbHost :: !String,
    dbPort :: !Int,
    dbUser :: !String,
    dbPass :: !String,
    dbDatabase :: !String
  }
  deriving (Show)

data Swagger = Swagger {swaggerHost :: String, swaggerPort :: Maybe Int}
  deriving (Show)

data HasqlSettings = HasqlSettings
  { hasqlSettingsPoolN :: !Int,
    hasqlSettingsTm :: !Double,
    hasqlSettingsResPerStripe :: !Int
  }
  deriving (Show)

data Env = Prod | Dev deriving (Show, Eq)

mkEnumConvertor ''Env

instance FromJSON Env where parseJSON = withText "Scaffold.Config:Env" (pure . toEnv . toS)

data StdoutFormat = Json | Bracket deriving (Show, Eq)

mkEnumConvertor ''StdoutFormat

instance FromJSON StdoutFormat where parseJSON = withText "Scaffold.Config:StdoutFormat" (pure . toStdoutFormat . toS)

data Katip = Katip
  { katipPath :: !FilePath,
    katipSeverity :: !String,
    katipVerbosity :: !String,
    katipEnv :: !Env,
    katipStdoutFormat :: !StdoutFormat
  }
  deriving (Show)

newtype ApiKeys = ApiKeys [(String, String)]
  deriving (Show)

newtype Service = Service {serviceApiKeys :: ApiKeys}
  deriving (Show)

data Minio = Minio
  { minioAccessKey :: !T.Text,
    minioSecretKey :: !T.Text,
    minioBucketPrefix :: !T.Text,
    minioHost :: !String,
    minioPort :: !String,
    minioLogBucket :: !String
  }
  deriving (Show)

data Telegram = Telegram
  { telegramBot :: !(Maybe T.Text),
    telegramChat :: !T.Text,
    telegramHost :: !T.Text,
    telegramEnv :: !Env
  }
  deriving (Show)

newtype ServerConnection = ServerConnection {serverConnectionPort :: Int}
  deriving (Show)

newtype Cors = Cors {corsOrigins :: (Maybe [T.Text])} deriving (Show)

newtype ServerError = ServerError {serverErrorMk500 :: Bool}
  deriving stock (Generic)
  deriving newtype (FromJSON)
  deriving stock (Show)

newtype Email = Email T.Text
  deriving stock (Generic)
  deriving newtype (FromJSON, ToJSON)
  deriving stock (Show)
  deriving (ToSchema)

data Config = Config
  { configDb :: !Db,
    configSwagger :: !Swagger,
    configHasql :: !HasqlSettings,
    configKatip :: !Katip,
    configService :: !Service,
    configMinio :: !Minio,
    configTelegram :: !Telegram,
    configServerConnection :: !ServerConnection,
    configCors :: !Cors,
    configServerError :: !ServerError,
    configJobFrequency :: !Int,
    configWebhook :: !T.Text
  }
  deriving (Show)

makeFields ''Config
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''Katip
makeFields ''Minio
makeFields ''Swagger
makeFields ''Telegram
makeFields ''ServerConnection
makeFields ''Cors

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''Katip
deriveFromJSON defaultOptions ''ApiKeys
deriveFromJSON defaultOptions ''Service
deriveFromJSON defaultOptions ''Minio
deriveFromJSON defaultOptions ''Telegram
deriveFromJSON defaultOptions ''ServerConnection
deriveFromJSON defaultOptions ''Cors
deriveFromJSON defaultOptions ''Swagger
deriveFromJSON defaultOptions ''Config

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FromJSON a => FilePath -> IO a
load path = decodeFileEither path >>= either throwIO pure
