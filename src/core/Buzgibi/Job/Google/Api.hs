{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Buzgibi.Job.Google.Api (Api, callApi) where

import Buzgibi.Job.Google.Token (obtainAccessToken, AccessToken (..))
import Buzgibi.EnvKeys (Google, url, tokenEmail, tokenUrl, tokenPk, key)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import Data.Kind (Type, Constraint)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Control.Lens ((^.))
import Data.String.Conv (toS)
import Data.Traversable (for)
import Data.Bifunctor (first)
import Control.Monad (join)
import Data.Proxy (Proxy (..))

type family Api (api :: Symbol) (req :: Type) (resp :: Type) :: Constraint

callApi :: 
  forall api req resp . 
  (KnownSymbol api, 
   Typeable api, 
   FromJSON resp,
   ToJSON req, 
   Api api req resp) =>
  Google ->
  HTTP.Manager ->
  HTTP.Method ->
  req ->
  IO (Either T.Text resp)
callApi cfg manager method body = do
  resp <- obtainAccessToken manager (cfg^.tokenUrl) (cfg^.tokenEmail) (cfg^.tokenPk)
  fmap (join . first toS) $ 
    for resp $ \(AccessToken token) -> do
      let uri = T.unpack $ (cfg^.url) <> "/" <> toS (symbolVal (Proxy @api)) <> "?key=" <> (cfg^.key)
      requestBuilder <- HTTP.parseRequest uri
      let hs = 
           [("x-goog-user-project", "buzgibi"), 
            (HTTP.hAuthorization, toS ("Bearer " <> token))]
      let bs = encode body
      let request = 
            requestBuilder {
              HTTP.method = method, 
              HTTP.requestHeaders = hs,
              HTTP.requestBody = 
              HTTP.RequestBodyLBS bs }
      response <- HTTP.httpLbs request manager
        
      let status = HTTP.statusCode $ HTTP.responseStatus response
      let body = toS $ HTTP.responseBody response
      pure $ if status == HTTP.statusCode HTTP.status200 
      then first toS $ eitherDecodeStrict @resp body 
      else Left $ toS $ show response  