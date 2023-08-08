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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Buzgibi.Api.Controller.User.Survey.Edit (controller, EditSurvey) where

import qualified Buzgibi.Transport.Model.Bark as Bark
import qualified Buzgibi.Statement.User.Survey as Survey
import Buzgibi.Api.Controller.User.Survey.Make (voices, mkBarkRequest)
import Buzgibi.EnvKeys (textTemp, waveformTemp, url, version, key)
import Buzgibi.Auth (AuthenticatedUser (..))
import Buzgibi.Transport.Response
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Katip.Controller
import Data.Int (Int64)
import BuildInfo (location)
import Katip
import Control.Lens
import Buzgibi.Api.Controller.Utils (withError)
import Data.Traversable (for)
import Data.Either.Combinators (maybeToRight)
import Database.Transaction
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import qualified Request as Request (make)
import qualified Network.HTTP.Types as HTTP
import Control.Lens.Iso.Extended (textbs)
import Data.Aeson (toJSON, eitherDecodeStrict)
import Data.Foldable (for_)
import Data.Maybe (isJust)

data Error = BarkCredentials404

instance Show Error where
  show BarkCredentials404 = "we cannot perform the request"

data EditSurvey = EditSurvey { editSurveySurvey :: T.Text }
     deriving stock (Generic)
     deriving stock (Show)
     deriving
        (ToJSON, FromJSON)
        via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor EditSurvey)]]
          EditSurvey

deriveToSchemaFieldLabelModifier ''EditSurvey [|modify (Proxy @EditSurvey)|]

controller :: AuthenticatedUser -> Int64 -> EditSurvey -> KatipControllerM (Response Bool)
controller _ _ EditSurvey {..}
  | T.length editSurveySurvey == 0 = return $ Warnings False [asError @T.Text "empty_survey"]
  | T.length editSurveySurvey > 180 = return $ Warnings False [asError @T.Text "survey_truncated_to_180"]
controller AuthenticatedUser {..} surveyIdent value@EditSurvey {..} = do 
  $(logTM) DebugS $ logStr @String $ $location <> " edit survey ---> key: " <> show surveyIdent <> ", value: " <> show value
  barkm <- fmap (^. katipEnv . bark) ask
  resp <- fmap (maybeToRight BarkCredentials404) $ 
    for barkm $ \bark -> do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      manager <- fmap (^. katipEnv . httpReqManager) ask
      webhook <- fmap (^. katipEnv . webhook) ask
      transactionM hasql $ do 
        draftIdentm <- statement Survey.insertDraft (ident, surveyIdent, editSurveySurvey)
        for_ draftIdentm $ \draftIdent -> do 
          idx <- liftIO $ randomRIO (0, 2)
          let voice = voices (bark^.textTemp) (bark^.waveformTemp) !! idx
          resp <- liftIO $ do
            Request.make
              (bark^.url) manager 
              [(HTTP.hAuthorization, "Token " <> (bark^.key.textbs))] 
              HTTP.methodPost $ 
              Left (Just (mkBarkRequest webhook (bark^.version) voice editSurveySurvey))
          case resp of
            Right (resp, _) -> do
              let bark_resp = eitherDecodeStrict @Bark.Response resp
              let mkBarkRecord ident st = 
                    Survey.Bark {
                      Survey.barkReq = toJSON $ mkBarkRequest webhook (bark^.version) voice editSurveySurvey,
                      Survey.barkStatus = st,
                      Survey.barkIdent = ident,
                      Survey.barkSurveyDraftId = draftIdent
                    }
              case bark_resp of
                Right resp -> 
                  statement Survey.insertBark $
                    mkBarkRecord (Bark.responseIdent resp) Survey.BarkSent
                Left err -> error $ "bark response resulted in error: " <> show err
            Left err -> error $ "bark response resulted in error: " <> show err
        pure $ isJust draftIdentm    
  return $ withError resp $ id