{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}

module Buzgibi.Api.Controller.User.GetHistory (controller, History, Status (..), HistoryItem (..)) where

import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import qualified Buzgibi.Statement.User.Survey as Survey
import Buzgibi.Api.Controller.Utils (withError)
import Katip.Controller
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Control.Lens
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))
import Database.Transaction
import Data.Coerce (coerce)
import Data.Int (Int32)
import TH.Mk (mkToSchemaAndJSON)
import Data.Aeson.WithField
import Data.Bifunctor (first, second)
import Data.Traversable (for)
import Network.Minio (runMinioWith, getObject, defaultGetObjectOptions, gorObjectStream)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.Combinators as Conduit
import Data.Conduit (runConduit, (.|))
import Data.Binary.Builder (fromByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (join)
import qualified Data.ByteString.Lazy as BL

data Status = InProcess | Done | Fail | Draft
  deriving stock (Generic, Show, Eq)

mkToSchemaAndJSON ''Status

data HistoryItem = 
     HistoryItem
     { 
        historyItemSurveyIdent :: !Int64,
        historyItemReportIdent :: !(Maybe Int64),
        historyItemName :: !T.Text,
        historyItemTimestamp :: !UTCTime,
        historyItemVoice :: !(Maybe T.Text)
     }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor HistoryItem)]]
          HistoryItem

deriveToSchemaFieldLabelModifier ''HistoryItem [|modify (Proxy @HistoryItem)|]

data History = 
     History 
     { 
        historyTotal :: !Int32,
        historyPerPage :: !Int32,
        historyItems :: ![WithField "status" Status HistoryItem]
     }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor History)]]
          History

deriveToSchemaFieldLabelModifier ''History [|modify (Proxy @History)|]


data Bark = Bark { barkHash :: T.Text, barkBucket :: T.Text }
     deriving stock (Generic, Show)
     deriving
       (ToJSON, FromJSON)
       via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Bark)]]
          Bark

controller :: AuthenticatedUser -> Maybe Int -> KatipControllerM (Response History)
controller user page = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let offset = maybe 1 fromIntegral page
  res <- fmap mkHistory $ transactionM hasql $ statement Survey.getHistory (coerce user, offset)
  res' <- fmap join $ for res $ \(xs, total) -> 
    case xs of 
      [] -> pure $ Right $ History 0 0 []
      items -> 
        fmap (fmap (History total 10) . sequence) $ 
          for items $ \(WithField bark item) -> 
            if getFirst item == Draft then
               case bark of 
                 Just (Bark {..}) ->
                  do Minio {..} <- fmap (^. katipEnv . minio) ask
                     bs <- liftIO $ runMinioWith minioConn $ do 
                        obj <- getObject barkBucket barkHash defaultGetObjectOptions
                        runConduit $
                           gorObjectStream obj .|
                           Conduit.map fromByteString .|
                           Conduit.sinkLazyBuilder
                     pure $ first show $ bs <&> \b -> 
                        flip second item $ \x -> 
                           x { historyItemVoice = 
                                 Just (decodeUtf8 (B64.encode (BL.toStrict b))) }
                 Nothing -> pure $ Right item      
            else pure $ Right item
  return $ withError res' id

mkHistory (Just (xs, total)) = 
   let xs' = sequence (map (fmap (second (first mkStatus)) . eitherDecode @(WithField "bark" (Maybe Bark) (WithField "status" Survey.Status HistoryItem)) . encode) xs)
   in fmap (,total) xs'
mkHistory _ = Right ([], 0)

mkStatus :: Survey.Status -> Status
mkStatus Survey.SurveyProcessed = Done
mkStatus Survey.Fail = Fail
mkStatus (Survey.TelnyxAppFailure _) = Fail
mkStatus Survey.Draft = Draft
mkStatus _ = InProcess