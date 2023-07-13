module Request (make) where

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import Data.String.Conv
import qualified Data.ByteString as B
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)

make ::
  ToJSON a =>
  T.Text
  -> HTTP.Manager
  -> [HTTP.Header]
  -> HTTP.Method
  -> Maybe a
  -> IO (Either B.ByteString (B.ByteString, HTTP.ResponseHeaders))
make url manager headers method body = do
 req <- HTTP.parseRequest $ T.unpack url 
 response <- 
   flip HTTP.httpLbs manager 
     req {
        HTTP.method = method, 
        HTTP.requestHeaders = headers,
        HTTP.requestBody = HTTP.RequestBodyLBS $ fromMaybe mempty $ fmap encode body
     }
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
   else Left response_body
