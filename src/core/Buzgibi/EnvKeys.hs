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

data Introduction = Introduction { introductionYn :: !T.Text, introductionFrom0To10 :: T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Introduction)]]
          Introduction

data Bark = 
     Bark 
     { 
       barkKey :: !T.Text, 
       barkVersion :: !T.Text, 
       barkUrl :: !T.Text,
       barkTextTemp :: !Double,
       barkWaveformTemp :: !Double,
       barkIntroduction :: !Introduction
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Bark)]]
          Bark

data Telnyx = 
     Telnyx 
     { telnyxUrl :: !T.Text, 
       telnyxKey :: !T.Text, 
       telnyxPhone :: !T.Text, 
       telnyxOutbound :: !T.Text,
       telnyxApppostfix :: !T.Text,
       telnyxMachine :: !T.Text
     } 
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Telnyx)]]
          Telnyx

data OpenAI = OpenAI { openAIUrl :: !T.Text, openAIKey :: !T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor OpenAI)]]
          OpenAI

data Google = 
     Google { 
      googleUrl :: !T.Text, 
      googleKey :: !T.Text, 
      googleClarifyingPrefix :: !T.Text, 
      googleTokenUrl :: !T.Text,
      googleTokenEmail :: !T.Text,
      googleTokenPk :: !T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Google)]]
          Google


data Person = Person { personEmail :: !T.Text, personPersonalization :: !T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Person)]]
          Person

data Sendgrid = 
     Sendgrid 
     { sendgridUrl :: !T.Text, 
       sendgridKey :: !T.Text, 
       sendgridIdentity :: !T.Text,
       sendgridPersons :: ![Person]  
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Sendgrid)]]
          Sendgrid

data Deepgram = Deepgram {  deepgramKey :: T.Text, deepgramUrl :: T.Text, deepgramLang :: T.Text, deepgramClarifyingPrefix :: !T.Text  } 
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Deepgram)]]
          Deepgram

data EnvKeys = EnvKeys
  { envKeysSendgrid :: !(Maybe Sendgrid),
    envKeysTelegramBot :: !(Maybe T.Text),
    envKeysCaptchaKey :: !(Maybe T.Text),
    envKeysGithub :: !(Maybe Github),
    envKeysBark :: !(Maybe Bark),
    envKeysTelnyx :: !(Maybe Telnyx),
    envKeysOpenAI :: !(Maybe OpenAI),
    envKeysGoogle :: !(Maybe Google),
    envKeysDeepgram :: !(Maybe Deepgram)
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
makeFields ''OpenAI
makeFields ''Sendgrid
makeFields ''Introduction
makeFields ''Google
makeFields ''Deepgram