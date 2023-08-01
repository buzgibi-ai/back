{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Buzgibi.Api.Http.Test (spec_api) where

import Data.Proxy
import Buzgibi.Api.File
import Buzgibi.Api.User (AuthApi (..), UserApi (..))
import Buzgibi.Api.Controller.User.MakeSurvey (Survey (..), Location (..))
import Buzgibi.Api.Controller.User.GetHistory (History (..), Status (..), HistoryItem (..))
import Buzgibi.Transport.Model.User (Credentials (..), AuthToken (..))

import Servant.API.Generic
import Servant.Swagger.Test
import Test.Hspec
import TH.Mk (mkArbitrary)

mkArbitrary ''AuthToken
mkArbitrary ''HistoryItem
mkArbitrary ''Status
mkArbitrary ''Location
mkArbitrary ''Survey
mkArbitrary ''History
mkArbitrary ''Credentials

spec_api :: Spec
spec_api =
  describe "Swagger spec for API" $ do
    context "ToJSON matches ToSchema (FileApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))
    context "ToJSON matches ToSchema (AuthApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy AuthApi))
    context "ToJSON matches ToSchema (UserApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy UserApi))