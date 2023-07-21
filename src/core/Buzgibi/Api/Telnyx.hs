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

module Buzgibi.Api.Telnyx (TelnyxApi, TelnyxApiCfg (..), callApi) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Client as HTTP
import Buzgibi.EnvKeys (Telnyx (..))
import Data.Typeable (Typeable, typeRep)
import qualified Data.Text as T
import Network.HTTP.Types.Header (ResponseHeaders)
import Data.Aeson (FromJSON, ToJSON)
import qualified Request as Request
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (methodPost, hAuthorization, hContentType)
import Control.Exception (try)
import Data.String.Conv (toS)
import Data.Bifunctor (first)
import Control.Monad (join)

type family TelnyxApi (api :: Symbol) (req :: Type) (resp :: Type) :: Constraint

data TelnyxApiCfg =
      TelnyxApiCfg 
      { telnyxCfg :: Telnyx,
        manager :: HTTP.Manager
      } 

callApi ::
  forall a b c e r . 
  (Typeable a, FromJSON c, ToJSON b, TelnyxApi a b c) => 
  TelnyxApiCfg ->
  b ->
  (T.Text -> (Either e r)) -> 
  ((c, ResponseHeaders) -> Either e r) -> 
  IO (Either e r)
callApi TelnyxApiCfg {..} request onError onOk = do
  let url = telnyxUrl telnyxCfg <> "/" <> toS (show (typeRep (Proxy @a)))
  let authH = (hAuthorization, toS ("Bearer " <> telnyxKey telnyxCfg))
  let contTypeH = (hContentType, "application/json")
  resp <- fmap (join . first (toS . show)) $ try @HttpException $ 
            Request.make @b url manager [authH, contTypeH] methodPost $ Just $ request
  return $ Request.withError @c resp onError onOk

