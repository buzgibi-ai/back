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


data Github = Github { githubKey :: !T.Text, githubRepos :: ![T.Text], githubTranslation :: T.Text } 
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Github)]]
          Github

data EnvKeys = EnvKeys
  { envKeysSendgrid :: !(Maybe T.Text),
    envKeysTelegramBot :: !(Maybe T.Text),
    envKeysCaptchaKey :: !(Maybe T.Text),
    envKeysGithub :: !(Maybe Github)
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