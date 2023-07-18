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
        "insertToken" =>> Buzgibi.Statement.User.Auth.insertToken
     ]
  , "Buzgibi.Statement.User.Survey" ==>
     [ "insert" =>> Buzgibi.Statement.User.Survey.insert,
       "insertBark" =>> Buzgibi.Statement.User.Survey.insertBark,
       "updateBark" =>> Buzgibi.Statement.User.Survey.updateBark,
       "insertVoice" =>> Buzgibi.Statement.User.Survey.insertVoice, 
       "getHistory" =>> Buzgibi.Statement.User.Survey.getHistory,
       "getPhoneMeta" =>> Buzgibi.Statement.User.Survey.getPhoneMeta,
       "insertPhones" =>> Buzgibi.Statement.User.Survey.insertPhones,
       "getVoiceObject" =>> Buzgibi.Statement.User.Survey.getVoiceObject,
       "insertShareLink" =>> Buzgibi.Statement.User.Survey.insertShareLink
     ]
  ]
