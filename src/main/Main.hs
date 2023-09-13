{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import qualified Buzgibi.App as App
import Buzgibi.Config
import Buzgibi.EnvKeys

import BuildInfo (gitCommit)
import qualified Cfg.SendGrid as SendGrid
import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens hiding (Unwrapped, Wrapped)
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.RWS.Strict (evalRWST)
import Data.Char (isUpper, toLower)
import Data.Default.Class
import Data.Foldable (for_)
import Data.Maybe
import Data.Monoid.Colorful (hGetTerm)
import qualified Data.Pool as Pool
import Data.String
import Data.Time.Clock.System
import Data.Traversable (for)
import GHC.Read
import qualified Hasql.Connection as HasqlConn
import Katip
import Katip.Scribes.Minio as Scribes.Minio
import qualified Katip.Scribes.Telegram as Scribes.Telegram
import Katip.Controller hiding (webhook)
import Network.HTTP.Client
  ( ManagerSettings
      ( managerConnCount,
        managerResponseTimeout
      ),
    responseTimeoutMicro,
  )
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Minio as Minio
import Options.Generic
import Pretty
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import System.IO
import Text.ParserCombinators.ReadPrec (pfail)
import qualified Text.Read.Lex as L
import Crypto.JOSE.JWK (JWK)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import Data.Either.Combinators (whenLeft)
import Data.Aeson (eitherDecode')
import Data.String.Conv (toS)

data PrintCfg = Y | N deriving stock (Generic)

instance Show PrintCfg where
  show Y = "y"
  show N = "n"

instance Read PrintCfg where
  readPrec =
    parens
      ( do
          L.Ident s <- lexP
          case s of
            "y" -> return Y
            "n" -> return N
            _ -> pfail
      )

instance ParseField PrintCfg

data Cmd w = Cmd
  { cfgPath :: w ::: FilePath <?> "config file path",
    localhost :: w ::: Maybe String <?> "override db host if needed, used along with port",
    localport :: w ::: Maybe Int <?> "override db port if needed",
    pathToKatip :: w ::: Maybe FilePath <?> "path to katip log",
    pathToJwk :: w ::: FilePath <?> "path to jwk",
    minioHost :: w ::: Maybe String <?> "minio host",
    minioPort :: w ::: Maybe String <?> "minio port",
    minioAccessKey :: w ::: String <?> "minio access key",
    minioSecretKey :: w ::: String <?> "minio secret key",
    swaggerHost :: w ::: Maybe String <?> "swagger host",
    swaggerPort :: w ::: Maybe Int <?> "swagger port",
    serverPort :: w ::: Maybe Int <?> "server port",
    printCfg :: w ::: Maybe PrintCfg <?> "whether config be printed",
    envPath :: w ::: Maybe FilePath <?> "file for storing sensitive data. it's used only in deployment",
    mute500 :: w ::: Maybe Bool <?> "how to render 500 error",
    buzgibiDbUser :: w ::: String <?> "db user",
    buzgibiDbPass :: w ::: String <?> "db pass",
    buzgibiDatabase :: w ::: String <?> "database"
  }
  deriving stock (Generic)

deriving instance Show (Cmd Unwrapped)

instance ParseRecord (Cmd Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { fieldNameModifier = toSnake
        }

-- |
--   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
--   the \"snake\" 'String'.
--   It splits an input value to chunks by 'isUpper' predicate,
--   then adds underscores to each element except the first.
--   Finally concats the result and convers it downcase.
toSnake :: String -> String
toSnake = map toLower . concat . underscores . splitR isUpper
  where
    underscores [] = []
    underscores (h : t) = h : map ('_' :) t
    splitR _ [] = []
    splitR p s =
      let go m s' =
            case break p s' of
              (b', []) -> [m : b']
              (b', x : xs) -> (m : b') : go x xs
       in case break p s of
            (b, []) -> [b]
            ([], h : t) -> go h t
            (b, h : t) -> b : go h t

main :: IO ()
main = do
  cmd@Cmd {..} <- unwrapRecord "buzgibi"
  print "------ Cmd: start ------"
  pPrint cmd
  print "------ Cmd: end ------"

  -- at this initialisation step we have to obtain sensitive data from env
  envKeys <- fmap join $ for envPath $ \path -> do
    cond <- doesFileExist path
    if cond
      then fmap Just $ Buzgibi.Config.load @EnvKeys path
      else return Nothing

  print "------ EnvKeys: start ------"
  pPrint envKeys
  print "------ EnvKeys: end ------"

  rawCfg <- Buzgibi.Config.load @Buzgibi.Config.Config cfgPath
  let cfg =
        rawCfg
          & db . host %~ (`fromMaybe` localhost)
          & db . port %~ (`fromMaybe` localport)
          & db . user .~ buzgibiDbUser
          & db . pass .~ buzgibiDbPass
          & db . database .~ buzgibiDatabase
          & katip . path %~ (\path -> maybe path (</> path) pathToKatip)
          & Buzgibi.Config.minio . host %~ (`fromMaybe` minioHost)
          & Buzgibi.Config.minio . port %~ (`fromMaybe` minioPort)
          & Buzgibi.Config.minio . accessKey .~ toS minioAccessKey
          & Buzgibi.Config.minio . secretKey .~ toS minioSecretKey
          & swagger . host %~ (`fromMaybe` swaggerHost)
          & swagger . port %~ (flip (<|>) swaggerPort)
          & serverConnection . port %~ (`fromMaybe` serverPort)

  for_ printCfg $
    \case
      Y ->
        do
          print "------ Cfg: start ------"
          pPrint cfg
          print "------ Cfg: end ------"
      N -> pure ()

  -- at this initialisation step we have to put sensitive data into the config

  term <- hGetTerm stdout
  hSetBuffering stdout NoBuffering

  let mkRawConn x =
        HasqlConn.settings
          (x ^. host . stext . textbs)
          (x ^. port . to fromIntegral)
          (x ^. Buzgibi.Config.user . stext . textbs)
          (x ^. pass . stext . textbs)
          (x ^. database . stext . textbs)

  hasqlpool <-
    Pool.newPool $
      Pool.defaultPoolConfig
      (do connRes <- HasqlConn.acquire (mkRawConn (cfg ^. db))
          case connRes of Left e -> error $ show e; Right conn -> pure conn
      )
      HasqlConn.release
      (cfg^.hasql.tm)
      (cfg^.hasql.resPerStripe)

  std <-
    mkHandleScribeWithFormatter
      (if cfg^.katip.stdoutFormat == Json 
       then jsonFormat 
       else bracketFormat)
      ColorIfTerminal
      stdout
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)
  tm <- fmap systemSeconds getSystemTime
  let katipFilePath = cfg ^. katip . path <> "/" <> show tm <> ".log"
  createDirectoryIfMissing True $ cfg ^. katip . path
  fileHdl <- openFile katipFilePath AppendMode

  mapM_ (`hSetEncoding` utf8) [stdout, stderr, fileHdl]

  let mkNm = Namespace [("<" ++ $(gitCommit) ++ ">") ^. stext]
  init_env <- initLogEnv mkNm (cfg ^. katip . Buzgibi.Config.env . isoEnv . stext . coerced)

  file <-
    mkHandleScribe
      (ColorLog True)
      fileHdl
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)

  manager <-
    Http.newTlsManagerWith
      Http.tlsManagerSettings
        { managerConnCount = 50,
          managerResponseTimeout =
            responseTimeoutMicro (5 * 1_000_000)
        }

  minioEnv <-
    flip Minio.mkMinioConn manager $
      Minio.setCreds
        ( Minio.CredentialValue
            (fromString (cfg^.Buzgibi.Config.minio.accessKey.from stext))
            (fromString (cfg^.Buzgibi.Config.minio.secretKey.from stext))
            Nothing
        )
        (fromString (cfg ^. Buzgibi.Config.minio . host <> ":" <> cfg ^. Buzgibi.Config.minio . port))

  telegramScribe <- 
    Scribes.Telegram.mkScribe
      manager
      (cfg ^. Buzgibi.Config.telegram & bot %~ (flip (<|>) (join $ fmap envKeysTelegramBot envKeys)))
      (permitItem WarningS)
      (cfg ^. katip . verbosity . from stringify)

  minioScribe <-
    Scribes.Minio.mkScribe
      minioEnv
      (cfg ^. Buzgibi.Config.minio . logBucket . stext)
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)

  let env =
        registerScribe "stdout" std defaultScribeSettings init_env >>=
          registerScribe "file" file defaultScribeSettings >>=
            registerScribe "minio" minioScribe defaultScribeSettings >>=
              registerScribe "telegram" telegramScribe defaultScribeSettings

  unEnv <- env
  
  print "------ katip scribes: start ------"
  print $ Map.keys $ unEnv^.logEnvScribes
  print "------ katip scribes: end ------"

  print "------ server is about to run --------"

  let appCfg =
        App.Cfg
        { cfgHost = cfg ^. swagger . host . coerced,
          cfgSwaggerPort = cfg ^. swagger . port,
          cfgServerPort = cfg ^. serverConnection . port,
          cfgCors = cfg ^. cors,
          cfgServerError = cfg ^. serverError,
          mute500 = mute500,
          ns = mkNm ,
          logEnv = unEnv,
          telnyxCfg = envKeys >>= envKeysTelnyx,
          openaiCfg = envKeys >>= envKeysOpenAI,
          manager = manager,
          minio = (minioEnv, cfg ^. Buzgibi.Config.minio . Buzgibi.Config.bucketPrefix),
          webhook = cfg^.webhook,
          jobFrequency = cfg^.jobFrequency,
          sendgridCfg = envKeys >>= envKeysSendgrid,
          gcCfg = envKeys >>= envKeysGoogle,
          deepgramCfg = envKeys >>= envKeysDeepgram
        }

  jwke <- liftIO $ fmap (eitherDecode' @JWK) $ B.readFile pathToJwk

  jwkRes <- for jwke $ \jwk -> do

    print "--------- jwk ------------"
    putStrLn $ (take 200 (show jwk)) <> ".... }"

    let katipMinio = Minio minioEnv (cfg ^. Buzgibi.Config.minio . Buzgibi.Config.bucketPrefix)
    let katipEnv = 
          KatipEnv 
          { katipEnvTerminal = term,
            katipEnvHasqlDbPool = hasqlpool,
              katipEnvHttpReqManager = manager,
              katipEnvApiKeys = (cfg ^. service . coerced),
              katipEnvMinio = katipMinio,
              katipEnvSendGrid = 
                envKeys >>= envKeysSendgrid <&> \sendgrid -> 
                  (sendgrid, SendGrid.configure (sendgrid^.url) (sendgrid^.key) ),
              katipEnvCaptchaKey = envKeys >>= envKeysCaptchaKey,
              katipEnvJwk = jwk,
              katipEnvGithub = envKeys >>= envKeysGithub,
              katipEnvBark = envKeys >>= envKeysBark,
              katipEnvTelnyx = envKeys >>= envKeysTelnyx,
              katipEnvWebhook = cfg^.webhook
          }

    let shutdownMsg = print "------ server is shut down --------"
    let runApp le = runKatipContextT le (mempty @LogContexts) mempty $ App.run appCfg
    bracket env (flip (>>) shutdownMsg . closeScribes) $ void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp

  whenLeft jwkRes $ print . ((<>) "jwk decode error")
