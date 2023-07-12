{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Buzgibi.Statement.User.Enquiry (create, Enquiry) where

import Data.Int (Int64)
import Data.Time (UTCTime)
import qualified Data.Text as T
import Hasql.TH
import Data.Default.Class
import GHC.Generics
import Data.Default.Class.Extended ()
import Data.Text.Extended ()
import TH.Mk
import Control.Lens
import Data.Maybe
import Database.Transaction (ParamsShow (..))
import qualified Hasql.Statement as HS
import Test.QuickCheck.Extended ()

data Status = Received | SentToBark | SentToTelnyx | Processed | Fail
  deriving Generic

instance Show Status where
    show Received = "received"
    show SentToBark = "bark" 
    show SentToTelnyx = "telnyx"
    show Processed = "processed"
    show Fail = "fail"

instance ParamsShow Status where
    render = show

instance Default Status where
    def = Received

mkArbitrary ''Status

data Enquiry = 
     Enquiry
     { enquiryUserId :: Int64,
     enquiryEnquiry :: T.Text,
     enquiryCreated :: UTCTime,
     enquiryProcessed :: Maybe UTCTime,
     enquiryStatus :: Status,
     enquiryLatitude :: Double,
     enquiryLongitude :: Double
     }
     deriving Generic
     deriving Show

instance Default Enquiry

mkEncoder ''Enquiry
mkArbitrary ''Enquiry

encode = fromMaybe (error "cannot encode Enquiry") . mkEncoderEnquiry

instance ParamsShow Enquiry where
  render = render . encode
 
create :: HS.Statement Enquiry (Maybe Int64)
create =
  lmap (\x -> encode x & _5 %~ (T.pack . show)) $
  [maybeStatement|
    insert into customer.enquiry 
    (user_id, enquiry, created, processed, enquiry_status, latitude, longitude) 
    values ($1 :: int8, $2 :: text, $3 :: timestamptz, $4 :: timestamptz?, $5 :: text, $6 :: float8, $7 :: float8)
    returning id :: int8|]