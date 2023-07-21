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

module Buzgibi.EnvKeys where

import Control.Lens
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import qualified Data.Text as T
import GHC.Generics

data Github = Github {githubKey :: !T.Text, githubRepos :: ![T.Text], githubTranslation :: T.Text}
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Github)]]
          Github

data Bark = Bark { barkKey :: !T.Text, barkVersion :: !T.Text, barkUrl :: !T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Bark)]]
          Bark

data Telnyx = Telnyx { telnyxUrl :: !T.Text, telnyxKey :: !T.Text, telnyxPhone :: !T.Text } 
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Telnyx)]]
          Telnyx

data EnvKeys = EnvKeys
  { envKeysSendgrid :: !(Maybe T.Text),
    envKeysTelegramBot :: !(Maybe T.Text),
    envKeysCaptchaKey :: !(Maybe T.Text),
    envKeysGithub :: !(Maybe Github),
    envKeysBark :: !(Maybe Bark),
    envKeysTelnyx :: !(Maybe Telnyx)
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor EnvKeys)]]
          EnvKeys

makeFields ''EnvKeys
makeFields ''Github
makeFields ''Bark
