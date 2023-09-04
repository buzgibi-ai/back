{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Request (make, withError, forConcurrentlyNRetry, retry) where

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import Data.String.Conv
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, encode, FromJSON, eitherDecodeStrict)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import qualified UnliftIO.Async as Async (pooledForConcurrentlyN)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Retry (retrying, constantDelay, limitRetries)

make ::
  ToJSON a =>
  T.Text
  -> HTTP.Manager
  -> [HTTP.Header]
  -> HTTP.Method
  -> Either (Maybe a) [Part]
  -> IO (Either (HTTP.Response BL.ByteString) (B.ByteString, HTTP.ResponseHeaders))
make url manager headers method bodye = do
 req_tmp <- HTTP.parseRequest $ T.unpack url
 let req =
      case bodye of 
        Left body ->
          pure $ 
            req_tmp {
              HTTP.method = method, 
              HTTP.requestHeaders = headers,
              HTTP.requestBody = 
                HTTP.RequestBodyLBS $ 
                  fromMaybe mempty $ 
                    fmap encode body
            }
        Right parts -> formDataBody parts req_tmp { HTTP.requestHeaders = headers }
 response <- flip HTTP.httpLbs manager =<< req
 let response_status = HTTP.statusCode $ HTTP.responseStatus response
 let response_body = toS $ HTTP.responseBody response
 return $
   if response_status ==
      HTTP.statusCode HTTP.status200 ||
    response_status == 
      HTTP.statusCode HTTP.status202 ||
    response_status == 
      HTTP.statusCode HTTP.status201
   then Right (response_body, HTTP.responseHeaders response)
   else Left response

withError :: 
  forall a b e. FromJSON a => 
  Either (HTTP.Response BL.ByteString) (B.ByteString, HTTP.ResponseHeaders) -> 
  (HTTP.Response BL.ByteString -> Either e b) -> 
  ((a,  HTTP.ResponseHeaders) -> Either e b) -> Either e b
withError (Right (bs, hs)) onError onOk = do
  case eitherDecodeStrict @a bs of 
    Right res -> onOk (res, hs)
    Left e -> error $ e <> ", raw bytes: " <> toS bs
withError (Left err) onError _ = onError err

forConcurrentlyNRetry 
  :: (MonadUnliftIO m, Traversable t) 
  => Int 
  -> Int 
  -> (b -> m Bool) 
  -> t a 
  -> (a -> m b) 
  -> m (t b)
forConcurrentlyNRetry threads delay shouldRetry xs go =
  Async.pooledForConcurrentlyN threads xs $ \x ->
    let retryPolicy = constantDelay (delay * 10 ^ 6) <> limitRetries 5
    in retrying retryPolicy (const shouldRetry) (const (go x))

retry :: MonadUnliftIO m => Int -> (a -> m Bool) -> m a -> m a
retry delay shouldRetry go =
  let retryPolicy = constantDelay (delay * 10 ^ 6) <> limitRetries 3
  in retrying retryPolicy (const shouldRetry) (const go)