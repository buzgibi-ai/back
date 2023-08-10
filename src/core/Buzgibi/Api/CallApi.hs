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

module Buzgibi.Api.CallApi (Api, ApiCfg (..), IsApi (..), methodPost, callApi) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Client as HTTP
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.Aeson (FromJSON, ToJSON)
import qualified Request as Request
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (methodPost, hAuthorization, hContentType, Method)
import Control.Exception (try)
import Data.String.Conv (toS)
import Data.Bifunctor (first)
import Control.Monad (join)
import Katip
import Data.Foldable (foldl')
import Network.HTTP.Client.MultipartFormData (Part)
import BuildInfo (location)

type family Api (api :: Symbol) (req :: Type) (resp :: Type) :: Constraint

data ApiCfg a =
      ApiCfg 
      { cfg :: a,
        manager :: HTTP.Manager,
        logger :: Severity -> LogStr -> IO ()
      } 

class IsApi a where
  getUrl :: a -> T.Text
  getKey :: a -> T.Text

callApi ::
  forall a b c e r cfg .
  (KnownSymbol a, Typeable a, FromJSON c, ToJSON b, Api a b c, IsApi cfg) => 
  ApiCfg cfg ->
  Either (Maybe b) [Part] ->
  Method ->
  [(T.Text, T.Text)] ->
  (T.Text -> (Either e r)) -> 
  ((c, ResponseHeaders) -> Either e r) -> 
  IO (Either e r)
callApi ApiCfg {..} request method queryXs onError onOk = do
  let url_tmp = getUrl cfg <> "/" <> toS (symbolVal (Proxy @a))
  let reduce tmp (needle, v) = T.replace needle v tmp
  let url | length queryXs > 0 = foldl' reduce url_tmp queryXs
          | otherwise = url_tmp
  let authH = (hAuthorization, toS ("Bearer " <> getKey cfg))
  let contTypeH = (hContentType, either (const "application/json") (const "multipart/form-data") request)

  logger DebugS $ logStr @String $ $location <> ": auth header ----> " <> toS (show authH)
  logger DebugS $ logStr @String $ $location <> ": url ----> " <> toS (show url)

  resp <- fmap (join . first (toS . show)) $ try @HttpException $ 
            Request.make @b url manager [authH, contTypeH] method request
  return $ Request.withError @c resp onError onOk

