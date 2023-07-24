{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.Frontend.Translate.Enum where

import Control.Lens
import Data.Default.Class
import GHC.Generics (Generic)
import TH.Mk

data Page = PageHome | PageAuth
  deriving stock (Generic)
  deriving (Enum)

instance Default Page where
  def = PageHome

data Menu = MenuHome | MenuSignUp | MenuSignIn
  deriving stock (Generic)
  deriving (Enum)

instance Default Menu where
  def = MenuSignUp

data Resource = ResourceUser | ResourceStub
  deriving stock (Generic)
  deriving (Enum)

instance Default Resource where
  def = ResourceUser

data Endpoints = EndpointsMakeSurvey | EndpointsStub
  deriving stock (Generic)
  deriving (Enum)

instance Default Endpoints where
  def = EndpointsMakeSurvey

mkToSchemaAndJSON ''Page
mkEnumConvertor ''Page

mkToSchemaAndJSON ''Menu
mkEnumConvertor ''Menu

mkToSchemaAndJSON ''Resource
mkEnumConvertor ''Resource

mkToSchemaAndJSON ''Endpoints
mkEnumConvertor ''Endpoints