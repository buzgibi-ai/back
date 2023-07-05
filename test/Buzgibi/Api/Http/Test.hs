{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Buzgibi.Api.Http.Test (spec_api) where

import Data.Proxy
import Buzgibi.Api.File
import Servant.API.Generic
import Servant.Swagger.Test
import Test.Hspec

spec_api :: Spec
spec_api =
  describe "Swagger spec for API v1" $ do
    context "ToJSON matches ToSchema (FileApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))