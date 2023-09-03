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

module Buzgibi.Api.CallApi (Api, ApiCfg (..), IsApi (..), methodPost, methodGet, callApi) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Client as HTTP
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.Aeson (FromJSON, ToJSON)
import qualified Request as Request
import Network.HTTP.Types (methodPost, methodGet, hAuthorization, hContentType, Method, Header)
import Katip
import Data.Foldable (foldl')
import Network.HTTP.Client.MultipartFormData (Part)
import BuildInfo (location)
import qualified Data.ByteString.Lazy as BL
import Data.String.Conv (toS)
import qualified Data.ByteString as B
import qualified Network.HTTP.Types as HTTP


type family Api (api :: Symbol) (req :: Type) (resp :: Type) :: Constraint

data ApiCfg a =
      ApiCfg 
      { cfg :: a,
        manager :: HTTP.Manager,
        logger :: Severity -> LogStr -> IO ()
      } 

class IsApi a where
  getUrl :: a -> T.Text
  getKey :: a -> IO T.Text

_callApi ::
  forall a b c cfg .
  (KnownSymbol a, Typeable a, FromJSON c, ToJSON b, Api a b c, IsApi cfg) => 
  ApiCfg cfg ->
  Either (Maybe b) [Part] ->
  Method ->
  [Header] ->
  [(T.Text, T.Text)] ->
  IO (Either (HTTP.Response BL.ByteString) (B.ByteString, HTTP.ResponseHeaders))
_callApi ApiCfg {..} request method hs queryXs = do
  let url_tmp = getUrl cfg <> "/" <> toS (symbolVal (Proxy @a))
  let reduce tmp (needle, v) = T.replace needle v tmp
  let url | length queryXs > 0 = foldl' reduce url_tmp queryXs
          | otherwise = url_tmp
  authKey <-  getKey cfg
  let authH = (hAuthorization, toS ("Bearer " <> authKey))
  let contTypeH = (hContentType, either (const "application/json") (const "multipart/form-data") request)

  logger DebugS $ logStr @String $ $location <> ": auth header ----> " <> toS (show ([authH, contTypeH] ++ hs))
  logger DebugS $ logStr @String $ $location <> ": url ----> " <> toS (show url)

  Request.make @b url manager ([authH, contTypeH] ++ hs) method request

callApi ::
  forall a b c e r cfg .
  (KnownSymbol a, Typeable a, FromJSON c, ToJSON b, Api a b c, IsApi cfg) => 
  ApiCfg cfg ->
  Either (Maybe b) [Part] ->
  Method ->
  [Header] ->
  [(T.Text, T.Text)] ->
  (T.Text -> (Either e r)) ->
  ((c, ResponseHeaders) -> Either e r) -> 
  IO (Either e r)
callApi cfg request method hs queryXs onError onOk = fmap (\resp -> Request.withError @c resp (onError . toS . HTTP.responseBody) onOk) $ _callApi @a @b @c cfg request method hs queryXs
{-# INLINE callApi #-}