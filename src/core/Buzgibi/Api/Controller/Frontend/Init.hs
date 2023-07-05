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
import Control.Lens.Iso.Extended (textbs)

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

data Init = Init
  { sha :: !T.Text,
    shaCss :: !T.Text,
    cookies :: ![T.Text],
    env :: !(Maybe Env),
    isJwtValid :: !Bool
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

controller :: AuthToken -> KatipControllerM (Response Init)
controller token = do
  key <- fmap (^. katipEnv . jwk) ask
  res <- liftIO $ validateJwt (defaultJWTSettings key) $ coerce token^.textbs
  return $ Ok $ defInit { isJwtValid = case res of Left _ -> False; _ -> True }
