{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Api.Controller.File.Download (controller, Option) where

import Buzgibi.Statement.File as File
import Buzgibi.Transport.Error
import Buzgibi.Transport.Id
import Buzgibi.Transport.Model.File
import qualified Buzgibi.Transport.Response as Response
import Conduit
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Coerce
import Data.Either.Combinators
import Data.Foldable
import Data.Int
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Traversable
import Database.Transaction
import Hash
import Katip
import Katip.Controller
import Network.HTTP.Types
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Header
import Network.Minio
import Network.Wai
import System.Process (readProcess)
import TH.Mk
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.String.Conv (toS)

data Option = Embedded | Raw deriving (Show)

mkEnumConvertor ''Option
mkParamSchemaEnum ''Option [|isoOption . stext . to String|]
mkFromHttpApiDataEnum ''Option [|from stext . from isoOption . to Right|]

imageMimeTypes = ["image/apng", "image/bmp", "image/gif", "image/x-icon", "image/jpeg", "image/png", "image/svg+xml", "image/tiff", "image/webp"]

controller :: Id "user" -> Option -> Id "file" -> Maybe Int -> Maybe Int -> KatipControllerM Application
controller userId option id width_m height_m = do
  $(logTM) DebugS (logStr (show (option, id, width_m, height_m)))
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id) ^. stext <> "} not found"
  meta <-
    fmap (maybeToRight (asError notFound)) $
      transactionM hasql $ statement File.getMetaForReport (userId, id)
  minioResp <- fmap join $ for meta $ \x -> do
    Minio {..} <- fmap (^. katipEnv . minio) ask
    let bucket = x ^. _4 . coerced
    r <- liftIO $ runMinioWith minioConn $ do
      o <- getObject bucket (x ^. _1 . coerced) defaultGetObjectOptions
      let size = oiSize (gorObjectInfo o)
      tm <- (toS . show . systemSeconds) <$> liftIO getSystemTime
      path <-
        runConduit $
          gorObjectStream o
            .| sinkSystemTempFile
              (x ^. _1 . coerced . from stext <> "_" <> tm)
      when (x ^. _3 . coerced @Mime @_ @T.Text @_ `elem` imageMimeTypes) $ do
        let size = do
              h <- height_m
              w <- width_m
              pure $ show h ++ "x" ++ show w
        for_ size $ \s -> liftIO $ readProcess "convert" [path, "-resize", s, path] mempty
      payload <- liftIO $ B.readFile path
      return (payload, size, x ^. _2 . coerced @Name @_ @T.Text @_, x ^. _3 . coerced @Mime @_ @T.Text @_, x ^. _5)
    return $ first (asError . (\e -> show e ^. stext)) r
  $(logTM) DebugS (logStr (show (second (^. _3) minioResp)))
  return $ \req resp -> case option of Embedded -> embedded req resp minioResp; Raw -> raw req resp minioResp

embedded ::
  Request ->
  (Response -> IO ResponseReceived) ->
  Either Error (B.ByteString, Int64, T.Text, T.Text, [T.Text]) ->
  IO ResponseReceived
embedded req resp _
  | requestMethod req /= methodGet =
      resp $
        responseLBS status200 [(H.hContentType, "application/json; charset=utf-8")] $
          encode @(Response.Response ()) $
            Response.Error (asError @T.Text ("only " <> toS methodGet <> " allowed"))
embedded _ resp minioResp = resp $
  case minioResp of
    Right minio ->
      responseLBS status200 [(hContentType, minio ^. _4 . textbs), (hContentDisposition, "inline")] $
        encode (Response.Ok $ decodeUtf8 $ B64.encode (minio ^. _1))
    Left e -> responseLBS status200 [] $ encode $ (Response.Error e :: Response.Response ())

raw ::
  Request ->
  (Response -> IO ResponseReceived) ->
  Either Error (B.ByteString, Int64, T.Text, T.Text, [T.Text]) ->
  IO ResponseReceived
raw req resp _ | requestMethod req /= methodGet = resp $ responseLBS status405 [] mempty
raw _ resp (Right minio) =
  let ext = 
       case minio ^._5 of 
         (ext:_) -> "." <> ext^.textbs
         [] -> ".a"
  in       
  resp
    $ responseLBS
      status200
      [ (hContentType, minio ^. _4 . textbs),
        ( hContentDisposition,
          "attachment;filename="
            <> (mkHash (minio ^. _3) ^. textbs) <> ext
        )
      ]
    $ (minio ^. _1 . from bytesLazy)
raw _ resp _ = resp $ responseLBS status404 [] "image not found"
