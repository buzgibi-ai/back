{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Buzgibi.Statement.User.Survey
  ( insert, 
    getHistory, 
    Survey (..), 
    Status (..), 
    NewBark (..),
    Category (..),
    AssessmentScore (..), 
    insertBark, 
    BarkStatus (..), 
    updateBark, 
    insertVoice,
    getPhoneMeta,
    insertPhones,
    getVoiceObject,
    insertShareLink,
    getUserByBarkIdent,
    getSurveyForTelnyxApp,
    insertTelnyxApp,
    getPhonesToCall,
    insertAppCall,
    insertAppPhone,
    insertVoiceUrlAppPhone
  ) where


import Buzgibi.Transport.Model.Telnyx (CallResponse, encodeCallResponse)
import Data.Int (Int64, Int32)
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
import Data.Bifunctor (second, first)
import qualified Data.Vector as V
import Data.String.Conv (toS)
import Data.Tuple.Extended (snocT, consT)

data Status = Received | ProcessedByBark | PickedByTelnyx | ProcessedByTelnyx | SurveyProcessed | Fail T.Text
  deriving Generic

instance Show Status where
    show Received = "received"
    show ProcessedByBark = "processed by bark" 
    show PickedByTelnyx = "telnyx app is created"
    show ProcessedByTelnyx = "processed by telnyx"
    show SurveyProcessed = "processed"
    show (Fail reason) = "fail: " <> toS reason

instance ParamsShow Status where
    render = show

instance Default Status where
    def = Received

mkArbitrary ''Status

data Category = 
       CustomerSatisfaction 
     | MarketResearch
     | ProductCampaign 
     | SocialResearch
     | PoliticalPoll
     deriving Generic
     deriving Show

mkToSchemaAndJSON ''Category

instance Default Category where
    def = CustomerSatisfaction

mkArbitrary ''Category

instance ParamsShow Category where
    render = show

data AssessmentScore = YN | ScaleOfTen
  deriving Generic
  deriving Show

mkToSchemaAndJSON ''AssessmentScore

instance Default AssessmentScore where
    def = YN

instance ParamsShow AssessmentScore where
    render = show

mkArbitrary ''AssessmentScore

data Survey = 
     Survey
     { surveyUserId :: Int64,
       surveySurvey :: T.Text,
       surveyStatus :: Status,
       surveyLatitude :: Double,
       surveyLongitude :: Double,
       surveyCategory :: Category,
       surveyAssessmentScore :: AssessmentScore,
       surveyPhones :: Int64
     }
     deriving Generic
     deriving Show

instance Default Survey

mkEncoder ''Survey
mkArbitrary ''Survey

encodeSurvey = fromMaybe (error "cannot encode Survey") . mkEncoderSurvey

instance ParamsShow Survey where
  render = render . encodeSurvey
 
insert :: HS.Statement Survey (Maybe Int64)
insert =
  lmap (\x -> 
    encodeSurvey x 
    & _3 %~ (T.pack . show) 
    & _6 %~ (T.pack . show) 
    & _7 %~ (T.pack . show)) $
  [maybeStatement|
    with 
      survey as (
        insert into customer.survey
        (user_id, survey, survey_status, latitude, longitude, category, survey_type)
        select
          id :: int8,
          $2 :: text, 
          $3 :: text, 
          $4 :: float8, 
          $5 :: float8,
          $6 :: text,
          $7 :: text
        from customer.profile
        where user_id = $1 :: int8
        returning id :: int8 as ident)
    insert into customer.survey_files 
    (survey_id, phones_id)
    select ident, $8 :: int8 from survey
    returning (select ident from survey) :: int8|]

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
     , barkSurveyId :: Int64
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
         (req, bark_status, bark_ident)
         values ($1 :: jsonb, $2 :: text, $3 :: text)
         on conflict on constraint bark__bark_ident_unique do
         update set
           bark_status = excluded.bark_status,
           bark_ident = foreign_api.bark.bark_ident 
           returning id :: int8 as ident)
      insert into customer.survey_bark
      (survey_id, bark_id)
      select $4 :: int8, ident :: int8 from bark
      on conflict on constraint survey_bark__survey_bark do nothing|]

updateBark :: HS.Statement (T.Text, BarkStatus) ()
updateBark = 
  lmap (second (T.pack . show)) $ 
  [resultlessStatement|
    update foreign_api.bark 
    set bark_status = $2 :: text, modified = now() 
    where bark_ident = $1 :: text|]

insertVoice :: HS.Statement (T.Text, BarkStatus, Int64, Status) ()
insertVoice =
  lmap (\x -> x & _2 %~ (T.pack . show) & _4 %~ (T.pack . show)) $ 
  [resultlessStatement|
    with
      survey_id as (
        select s.id :: int8
        from customer.survey as s
        inner join customer.survey_bark as sb
        on s.id = sb.survey_id
        inner join foreign_api.bark as b
        on sb.bark_id = b.id 
        where b.bark_ident = $1 :: text),
      bark as 
      (update foreign_api.bark
       set bark_status = $2 :: text, 
           modified = now() 
       where bark_ident = $1 :: text
       returning id :: int8 as ident),
      survey_bark as 
      (update customer.survey_bark
       set voice_id = $3 :: int8
       where bark_id = (select ident from bark)
       returning 1 :: int8 as ident),
      survey as
      (update customer.survey
       set survey_status = $4 :: text
       where id = (select id :: int8 from survey_id)
       returning 1 :: int8 as ident)
      select ident :: int8 from survey_bark union select ident :: int8 from survey|]

getHistory :: HS.Statement (Int64, Int32) (Maybe ([Value], Int32))
getHistory =
  rmap (fmap (first V.toList)) $
  [maybeStatement|
    with 
      tbl as 
        (select
           distinct on (f.id, e.survey, f.created)
           f.id :: int8 as ident,
           e.survey :: text as title,
           f.created :: timestamptz
         from customer.profile as p
         inner join customer.survey as e
         on p.id = e.user_id 
         left join customer.survey_bark as eb
         on e.id = eb.survey_id
         inner join storage.file as f
         on eb.voice_id = f.id
         where p.user_id = $1 :: int8 and eb.voice_id is not null
         group by f.id, e.survey, f.created
         order by f.id desc),
      total as (select count(*) from tbl),
      history as (select * from tbl offset (($2 :: int4 - 1) * 10) limit 10)
    select 
      array_agg(
        jsonb_build_object(
          'ident', ident, 
          'name', title, 
          'timestamp', created)) :: jsonb[], 
      (select * from total) :: int4 as cnt 
    from history group by cnt|]

getPhoneMeta :: HS.Statement Int64 (T.Text, T.Text)
getPhoneMeta = 
  [singletonStatement|
    select 
      f.bucket :: text,
      f.hash :: text
    from customer.survey_files as sf
    inner join storage.file as f
    on sf.phones_id = f.id
    where sf.survey_id = $1 :: int8|]

insertPhones :: HS.Statement (Int64, [T.Text]) ()
insertPhones = 
  lmap (second V.fromList) $ 
  [resultlessStatement|
    insert into customer.survey_phones
    (survey_id, phone)
    select $1 :: int8, phone :: text from unnest($2 :: text[]) phone|]

getVoiceObject :: HS.Statement T.Text (Maybe (T.Text, T.Text, T.Text, [T.Text]))
getVoiceObject = 
  rmap (fmap (\x -> x & _4 %~ V.toList)) $
  [maybeStatement|
    select 
      f.hash :: text,
      f.bucket :: text,
      s.survey :: text,
      array(select trim(both '"' from cast(el as text)) 
            from json_array_elements(f.exts) as el) :: text[]
    from foreign_api.bark as b
    inner join customer.survey_bark as sb
    on b.id = sb.bark_id
    inner join customer.survey as s
    on s.id = sb.survey_id
    inner join storage.file as f
    on sb.voice_id = f.id
    where b.bark_ident = $1 :: text|]

insertShareLink :: HS.Statement (T.Text, T.Text) ()
insertShareLink = 
  [resultlessStatement|
    insert into customer.voice_share_link 
    (bark_id, share_link_url, expires_at)
    select id :: int8, $2 :: text, now()
    from foreign_api.bark 
    where bark_ident = $1 :: text|]

getUserByBarkIdent :: HS.Statement T.Text (Maybe Int64)
getUserByBarkIdent =
  [maybeStatement|
    select au.id :: int8
    from auth.user as au
    inner join customer.profile as cp
    on au.id = cp.user_id
    left join customer.survey as cs
    on cp.id = cs.user_id
    left join customer.survey_bark as sb
    on cs.id = sb.survey_id
    inner join foreign_api.bark as b
    on b.id = sb.bark_id
    where b.bark_ident = $1 :: text|]

getSurveyForTelnyxApp :: HS.Statement () [(Int64, T.Text)]
getSurveyForTelnyxApp = 
  dimap (const (toS (show ProcessedByBark))) V.toList $ 
  [vectorStatement|
    select 
      s.id :: int8,
      ('user' || cast(u.id as text) || '_' || 'survey' || cast(s.id as text)) :: text
    from customer.survey as s
    inner join customer.profile as p
    on s.user_id = p.id
    inner join auth.user as u
    on p.user_id = u.id
    where survey_status = $1 :: text|]

insertTelnyxApp :: HS.Statement (Int64, T.Text, T.Text) ()
insertTelnyxApp =
  lmap (toS (show PickedByTelnyx) `snocT`)
  [resultlessStatement|
    with 
      survey as (
        update customer.survey
        set survey_status = $4 :: text
        where id = $1 :: int8),
      app as (
        insert into foreign_api.telnyx_app 
        (telnyx_ident, application_name) 
        values ($3 :: text, $2 :: text)
        returning id :: int8)
      insert into customer.phone_telnyx_app
      (telnyx_id, phone_id)
      select 
        (select * from app) :: int8,
        sp.id :: int8
      from customer.survey as s 
      inner join customer.survey_phones as sp
      on s.id = sp.survey_id
      where s.id = $1 :: int8|]

getPhonesToCall :: HS.Statement () [(Int64, T.Text, T.Text, [T.Text])]
getPhonesToCall =
  dimap 
    (const (toS (show PickedByTelnyx))) 
    (V.toList . fmap (\x -> x & _4 %~ V.toList)) $
  [vectorStatement|
    select
      distinct on (t.id, t.telnyx_ident, vsl.share_link_url)
      t.id :: int8,
      t.telnyx_ident :: text,
      vsl.share_link_url :: text,
      array_agg(sp.phone) :: text[]
    from customer.survey as s
    inner join customer.survey_bark as b
    on s.id = b.survey_id
    inner join customer.voice_share_link as vsl
    on b.bark_id = vsl.bark_id
    inner join customer.survey_phones as sp
    on s.id = sp.survey_id
    inner join customer.phone_telnyx_app as pt
    on sp.id = pt.phone_id
    inner join foreign_api.telnyx_app as t
    on pt.telnyx_id = t.id
    where s.survey_status = $1 :: text
    group by t.id, t.telnyx_ident, vsl.share_link_url|]

insertAppCall :: HS.Statement (Int64, CallResponse) ()
insertAppCall = 
  lmap (\(x, y) -> x `consT` encodeCallResponse y) $ 
  [resultlessStatement|
    insert into foreign_api.telnyx_app_call
    (telnyx_app_id, record_type, call_session_id, call_leg_id, call_control_id, is_alive)
    values ($1 :: int8, $2 :: text, $3 :: text, $4 :: text, $5 :: text, $6 :: boolean)|]

insertAppPhone :: HS.Statement (T.Text, T.Text, T.Text, T.Text) ()
insertAppPhone =
  [resultlessStatement|
    insert into foreign_api.telnyx_app_call_phone
    (telnyx_app_call_id, call_from, call_to, call_status)
    select 
      app.id :: int8,
      $2 :: text,
      $3 :: text,
      $4 :: text
    from foreign_api.telnyx_app as app
    inner join foreign_api.telnyx_app_call as call
    on app.id = call.telnyx_app_id
    where app.telnyx_ident = $1 :: text|]

insertVoiceUrlAppPhone :: HS.Statement (T.Text, T.Text, Value) ()
insertVoiceUrlAppPhone =
  [resultlessStatement|
     update foreign_api.telnyx_app_call_phone
     set recording_urls = $3 :: jsonb
     where 
       telnyx_app_call_id = (
        select app.id :: int8
        from foreign_api.telnyx_app as app
        inner join foreign_api.telnyx_app_call as call
        on app.id = call.telnyx_app_id
        where app.telnyx_ident = $1 :: text and call.call_leg_id = $2 :: text)|]