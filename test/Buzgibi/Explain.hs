{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Buzgibi.Explain (spec_explain) where

import qualified Buzgibi.Statement.File
import qualified Buzgibi.Statement.User.Auth 
import qualified Buzgibi.Statement.User.Survey
import qualified Buzgibi.Statement.User.Notification

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Positions
import Database.Migration.Test
import GHC.Generics
import Hasql.Decoders
import Hasql.Session
import Hasql.Statement
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Test.Hspec.Expectations.Lifted
import Test.QuickCheck (Arbitrary (arbitrary), generate)

spec_explain :: Spec
spec_explain =
  describeHasql
    [migrate]
    Nothing
    "explain"
    $ for_ explainTests
    $ \(modl, tests) ->
      context modl $
        for_ tests $
          \(name, ST st) ->
            itHasql name $ do
              let st' =
                    st
                      & position @1 %~ ("explain " <>)
                      & position @3 .~ noResult
              input <- liftIO $ generate arbitrary
              statement input st' >>= (`shouldBe` ())

deriving instance Generic (Statement a b)

-- | Existential wrapper for the query
data ST = forall a b. Arbitrary a => ST (Statement a b)

(==>) a b = (a, b)

(=>>) a b = (a, ST b)

-- | List of all database queries.
explainTests :: [(String, [(String, ST)])]
explainTests =
  [ "Buzgibi.Statement.File" ==>
      [ "save" =>> Buzgibi.Statement.File.save,
        "getMetaForReport" =>> Buzgibi.Statement.File.getMetaForReport,
        "getMeta" =>> Buzgibi.Statement.File.getMeta,
        "delete" =>> Buzgibi.Statement.File.delete,
        "getHashWithBucket" =>> Buzgibi.Statement.File.getHashWithBucket,
        "patch" =>> Buzgibi.Statement.File.patch
      ]
  , "Buzgibi.Statement.User.Auth" ==>
     [  "insertUser" =>> Buzgibi.Statement.User.Auth.insertUser,
        "insertJwt" =>> Buzgibi.Statement.User.Auth.insertJwt,
        "getUserIdByEmail" =>> Buzgibi.Statement.User.Auth.getUserIdByEmail,
        "logout" =>> Buzgibi.Statement.User.Auth.logout,
        "insertToken" =>> Buzgibi.Statement.User.Auth.insertToken,
        "insertConfirmationLink" =>> Buzgibi.Statement.User.Auth.insertConfirmationLink,
        "confirmEmail" =>> Buzgibi.Statement.User.Auth.confirmEmail,
        "resendLink" =>> Buzgibi.Statement.User.Auth.resendLink,
        "insertNewPassword" =>> Buzgibi.Statement.User.Auth.insertNewPassword,
        "checkToken" =>> Buzgibi.Statement.User.Auth.checkToken
     ]
  , "Buzgibi.Statement.User.Survey" ==>
     [ "insert" =>> Buzgibi.Statement.User.Survey.insert,
       "insertBark" =>> Buzgibi.Statement.User.Survey.insertBark,
       "updateBark" =>> Buzgibi.Statement.User.Survey.updateBark,
       "insertVoiceBark" =>> Buzgibi.Statement.User.Survey.insertVoiceBark, 
       "getHistory" =>> Buzgibi.Statement.User.Survey.getHistory,
       "getPhoneMeta" =>> Buzgibi.Statement.User.Survey.getPhoneMeta,
       "insertPhones" =>> Buzgibi.Statement.User.Survey.insertPhones,
       "getVoiceObject" =>> Buzgibi.Statement.User.Survey.getVoiceObject,
       "insertShareLink" =>> Buzgibi.Statement.User.Survey.insertShareLink,
       "getUserByBarkIdent" =>> Buzgibi.Statement.User.Survey.getUserByBarkIdent,
       "getSurveyForTelnyxApp" =>> Buzgibi.Statement.User.Survey.getSurveyForTelnyxApp,
       "insertTelnyxApp" =>> Buzgibi.Statement.User.Survey.insertTelnyxApp,
       "getPhonesToCall" =>> Buzgibi.Statement.User.Survey.getPhonesToCall,
       "insertAppCall" =>> Buzgibi.Statement.User.Survey.insertAppCall,
       "getUserByAppIdent" =>> Buzgibi.Statement.User.Survey.getUserByAppIdent,
       "insertVoiceTelnyx" =>> Buzgibi.Statement.User.Survey.insertVoiceTelnyx,
       "insertTranscription" =>> Buzgibi.Statement.User.Survey.insertTranscription,
       "getSurveysForSA" =>> Buzgibi.Statement.User.Survey.getSurveysForSA,
       "insertSA" =>> Buzgibi.Statement.User.Survey.insertSA,
       "getSurveyForReport" =>> Buzgibi.Statement.User.Survey.getSurveyForReport,
       "saveReport" =>> Buzgibi.Statement.User.Survey.saveReport,
       "invalidatePhones" =>> Buzgibi.Statement.User.Survey.invalidatePhones,
       "getSurveysForTranscription" =>> Buzgibi.Statement.User.Survey.getSurveysForTranscription,
       "updateAppCall" =>> Buzgibi.Statement.User.Survey.updateAppCall,
       "insertHangupCall" =>> Buzgibi.Statement.User.Survey.insertHangupCall,
       "checkAfterInvalidate" =>> Buzgibi.Statement.User.Survey.checkAfterInvalidate,
       "checkAfterWebhook" =>> Buzgibi.Statement.User.Survey.checkAfterWebhook,
       "failTelnyxApp" =>> Buzgibi.Statement.User.Survey.failTelnyxApp,
       "insertDraft" =>> Buzgibi.Statement.User.Survey.insertDraft,
       "submit" =>> Buzgibi.Statement.User.Survey.submit,
       "getVoiceLinkByCallLegId" =>> Buzgibi.Statement.User.Survey.getVoiceLinkByCallLegId,
       "checkAfterTranscription" =>> Buzgibi.Statement.User.Survey.checkAfterTranscription,
       "insertStat" =>> Buzgibi.Statement.User.Survey.insertStat,
       "getDailyPhoneStat" =>> Buzgibi.Statement.User.Survey.getDailyPhoneStat,
       "setInsufficientFund" =>> Buzgibi.Statement.User.Survey.setInsufficientFund,
       "detectStuckCalls" =>> Buzgibi.Statement.User.Survey.detectStuckCalls,
       "checkAfterWebhookAll" =>> Buzgibi.Statement.User.Survey.checkAfterWebhookAll
     ]
  ,  "Buzgibi.Statement.User.Notification" ==> 
     [ "get" =>> Buzgibi.Statement.User.Notification.get,
       "insert" =>> Buzgibi.Statement.User.Notification.insert
     ]
  ]