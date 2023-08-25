{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}

module Buzgibi.Api.Controller.Auth.Password.Create (controller, NewPassword) where

import qualified Buzgibi.Statement.User.Auth as Auth
import Buzgibi.Transport.Response (Response (Ok))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import Data.Typeable (typeRep)
import Data.Proxy (Proxy (..))
import Data.Char (toLower)
import Data.List (stripPrefix)
import Katip.Controller
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)

data NewPassword = NewPassword { newPasswordPassword :: Text, newPasswordKey :: Text }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor NewPassword)]]
          NewPassword

deriveToSchemaFieldLabelModifier
  ''NewPassword
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @NewPassword))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: NewPassword -> KatipControllerM (Response Bool)
controller NewPassword {..} = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap Ok $ transactionM hasql $ statement Auth.insertNewPassword (newPasswordPassword, newPasswordKey)