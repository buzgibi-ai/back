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

module Buzgibi.Job.Deepgram.Api (callApi) where

import qualified Data.Text as T
import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Data.String.Conv (toS)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL

callApi :: 
  forall resp .
  FromJSON resp =>
  T.Text -> 
  T.Text ->
  HTTP.Manager ->
  BL.ByteString ->
  IO (Either T.Text resp)
callApi url token manager body = do
  requestBuilder <- HTTP.parseRequest $ toS url
  let hs = [(HTTP.hAuthorization, toS ("Token " <> token)), ("accept", "application/json"), ("content-type", "application/json")]
  let request = 
        requestBuilder {
          HTTP.method = HTTP.methodPost, 
          HTTP.requestHeaders = hs,
          HTTP.requestBody =
          HTTP.RequestBodyLBS body }
  response <- HTTP.httpLbs request manager
        
  let status = HTTP.statusCode $ HTTP.responseStatus response
  let body = toS $ HTTP.responseBody response
  pure $ if status == HTTP.statusCode HTTP.status200 
    then first toS $ eitherDecodeStrict @resp body 
    else Left $ toS $ show response