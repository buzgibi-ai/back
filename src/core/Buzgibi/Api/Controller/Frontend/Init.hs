{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Buzgibi.Api.Controller.Frontend.Init (controller, Init) where

import Buzgibi.Transport.Model.User (AuthToken (..))
import Buzgibi.Transport.Response
import Control.Lens
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.Default.Class
import Data.Default.Class.Extended ()
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Text.Extended ()
import Data.Typeable (typeRep)
import GHC.Generics hiding (from, to)
import KatipController hiding (Service)
import Buzgibi.Auth (validateJwt)
import Servant.Auth.Server (defaultJWTSettings)
import Data.Coerce
import Control.Monad.IO.Class
import Control.Lens.Iso.Extended (textbs, jsonb, stext)
import TH.Mk
import Data.Traversable (for)

data Env = Env
  { envToTelegram :: !Bool,
    envIsCaptcha :: !Bool,
    envLogLevel :: !T.Text
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor Env)]]
          Env

instance Default Env

deriveToSchemaFieldLabelModifier
  ''Env
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @Env))
       in (fromMaybe s (stripPrefix (toLower head : tail) s)) & ix 0 %~ toLower
    |]


data JWTStatus = Valid | Invalid | Skip
  deriving stock (Generic)
  deriving (Enum)

instance Default JWTStatus where
  def = Valid

mkToSchemaAndJSON ''JWTStatus
mkEnumConvertor ''JWTStatus
mkParamSchemaEnum ''JWTStatus [|isoJWTStatus . jsonb|]
mkFromHttpApiDataEnum ''JWTStatus [|from stext . from isoJWTStatus . to Right|]

data Init = Init
  { sha :: !T.Text,
    shaCss :: !T.Text,
    cookies :: ![T.Text],
    env :: !(Maybe Env),
    isJwtValid :: !JWTStatus
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[OmitNothingFields True, FieldLabelModifier '[UserDefined (StripConstructor Init)]]
          Init

instance Default Init

deriveToSchemaFieldLabelModifier
  ''Init
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @Init))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

defInit = Init def def def def def

controller :: Maybe AuthToken -> KatipControllerM (Response Init)
controller token = do
  tokenResp <- for token $ \tk -> do
    key <- fmap (^. katipEnv . jwk) ask
    res <- liftIO $ validateJwt (defaultJWTSettings key) $ coerce tk^.textbs
    return $ case res of Left _ -> Invalid; _ -> Valid  
  return $ Ok $ defInit { isJwtValid = fromMaybe Skip tokenResp }