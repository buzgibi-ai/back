{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Buzgibi.Api.Telnyx (TelnyxApi, TelnyxApiCfg (..), methodPost, callApi) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Client as HTTP
import Buzgibi.EnvKeys (Telnyx (..))
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

type family TelnyxApi (api :: Symbol) (req :: Type) (resp :: Type) :: Constraint

data TelnyxApiCfg =
      TelnyxApiCfg 
      { telnyxCfg :: Telnyx,
        manager :: HTTP.Manager,
        logger :: Severity -> LogStr -> IO ()
      } 

callApi ::
  forall a b c e r .
  (KnownSymbol a, Typeable a, FromJSON c, ToJSON b, TelnyxApi a b c) => 
  TelnyxApiCfg ->
  b ->
  Method ->
  [(T.Text, T.Text)] ->
  (T.Text -> (Either e r)) -> 
  ((c, ResponseHeaders) -> Either e r) -> 
  IO (Either e r)
callApi TelnyxApiCfg {..} request method queryXs onError onOk = do
  let url_tmp = telnyxUrl telnyxCfg <> "/" <> toS (symbolVal (Proxy @a))
  let reduce tmp (needle, v) = T.replace needle v tmp
  let url | length queryXs > 0 = foldl' reduce url_tmp queryXs
          | otherwise = url_tmp
  let authH = (hAuthorization, toS ("Bearer " <> telnyxKey telnyxCfg))
  let contTypeH = (hContentType, "application/json")

  logger DebugS $ logStr $ "Buzgibi.Api.Telnyx: url ----> " <> url

  resp <- fmap (join . first (toS . show)) $ try @HttpException $ 
            Request.make @b url manager [authH, contTypeH] method $ Just $ request
  return $ Request.withError @c resp onError onOk

