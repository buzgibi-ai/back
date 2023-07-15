{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Buzgibi.Statement.User.Enquiry 
  ( insert, 
    getHistory, 
    Enquiry (..), 
    Status (..), 
    NewBark (..), 
    insertBark, 
    BarkStatus (..), 
    updateBark, 
    insertVoice
  ) where

import Data.Int (Int64)
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
import Data.Aeson.Types (Value)
import Data.Bifunctor (second)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V

data Status = Received | SentToBark | SentToTelnyx | EnquiryProcessed | Fail
  deriving Generic

instance Show Status where
    show Received = "received"
    show SentToBark = "bark" 
    show SentToTelnyx = "telnyx"
    show EnquiryProcessed = "processed"
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
       enquiryStatus :: Status,
       enquiryLatitude :: Double,
       enquiryLongitude :: Double
     }
     deriving Generic
     deriving Show

instance Default Enquiry

mkEncoder ''Enquiry
mkArbitrary ''Enquiry

encodeEnquiry = fromMaybe (error "cannot encode Enquiry") . mkEncoderEnquiry

instance ParamsShow Enquiry where
  render = render . encodeEnquiry
 
insert :: HS.Statement Enquiry (Maybe Int64)
insert =
  lmap (\x -> encodeEnquiry x & _3 %~ (T.pack . show)) $
  [maybeStatement|
    insert into customer.enquiry 
    (user_id, enquiry, enquiry_status, latitude, longitude)
    values ($1 :: int8, $2 :: text, $3 :: text, $4 :: float8, $5 :: float8)
    returning id :: int8|]

data BarkStatus = BarkSent | BarkStart | BarkProcessed | BarkFail
    deriving Generic

instance Show BarkStatus where
  show BarkSent = "sent"
  show BarkProcessed = "processed"
  show BarkFail = "fail"
  show BarkStart = "start"

instance ParamsShow BarkStatus where
    render = show

mkArbitrary ''BarkStatus

data NewBark = 
     Bark 
     { barkReq :: Value
     , barkStatus :: BarkStatus
     , barkIdent :: T.Text
     , barkEnquiryId :: Int64
     }
    deriving Generic
    deriving Show

mkEncoder ''NewBark
mkArbitrary ''NewBark

encodeNewBark = fromMaybe (error "cannot encode NewBark") . mkEncoderNewBark

instance ParamsShow NewBark where
  render = render . encodeNewBark

insertBark :: HS.Statement NewBark ()
insertBark = 
    lmap (\x -> encodeNewBark x & _2 %~ (T.pack . show)) $
    [resultlessStatement|
      with bark as 
        (insert into foreign_api.bark
         (req, bark_status, bark_ident, enquiry_id)
         values ($1 :: jsonb, $2 :: text, $3 :: text, $4 :: int8)
         on conflict on constraint bark__bark_ident_unique do
         update set
           bark_status = excluded.bark_status,
           bark_ident = foreign_api.bark.bark_ident 
           returning enquiry_id :: int8, id :: int8 as ident)
      insert into customer.enquiry_bark
      (enquiry_id, bark_id)
      select enquiry_id, ident :: int8 from bark
      on conflict on constraint enquiry_bark__enquiry_bark do nothing|]

updateBark :: HS.Statement (T.Text, BarkStatus) ()
updateBark = 
  lmap (second (T.pack . show)) $ 
  [resultlessStatement|
    update foreign_api.bark set bark_status = $2 :: text, modified = now() where bark_ident = $1 :: text|]

insertVoice :: HS.Statement (T.Text, BarkStatus, Int64) ()
insertVoice = 
  lmap (\x -> x & _2 %~ (T.pack . show)) $ 
  [resultlessStatement|
    with bark as 
    (update foreign_api.bark 
     set bark_status = $2 :: text, 
         modified = now() 
     where bark_ident = $1 :: text
     returning id :: int8 as ident)
    update customer.enquiry_bark
    set voice_id = $3 :: int8
    where bark_id = (select ident from bark)|]

getHistory :: HS.Statement Int64 [(Int64, T.Text, UTCTime)]
getHistory = 
  rmap V.toList $
  [vectorStatement|
     select 
      f.id :: int8 as ident,
      f.title :: text,
      f.created :: timestamptz 
     from customer.enquiry as e
     left join customer.enquiry_bark as eb
     on e.id = eb.enquiry_id
     inner join storage.file as f
     on eb.voice_id = f.id
     where e.user_id = $1 :: int8 and eb.voice_id is not null|]