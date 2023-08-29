{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Buzgibi.Statement.User.Survey
  ( insert, 
    InsertSurveyKeys (..),
    getHistory, 
    Survey (..), 
    Status (..), 
    NewBark (..),
    Category (..),
    AssessmentScore (..), 
    insertBark, 
    BarkStatus (..), 
    updateBark, 
    insertVoiceBark,
    getPhoneMeta,
    insertPhones,
    getVoiceObject,
    insertShareLink,
    getUserByBarkIdent,
    getSurveyForTelnyxApp,
    insertTelnyxApp,
    getPhonesToCall,
    insertAppCall,
    CallStatus (..),
    getUserByAppIdent,
    insertVoiceTelnyx,
    getSurveysForTranscription,
    OpenAITranscription (..),
    insertTranscription,
    mkTranscriptionOk,
    mkTranscriptionFailure,
    OpenAISA (..),
    getSurveysForSA,
    insertSA,
    getSurveyForReport,
    saveReport,
    SurveyForReportItem (..),
    invalidatePhones,
    PhoneToCall (..),
    updateAppCall,
    insertHangupCall,
    checkAfterInvalidate,
    checkAfterWebhook,
    failTelnyxApp,
    insertDraft,
    submit,
    getVoiceLinkByCallLegId,
    checkAfterTranscription,
    insertStat,
    getDailyPhoneStat,
    DailyPhoneStat (..),
    setInsufficientFund,
    InsertDraft (..)
  ) where


import Data.Int (Int64, Int32)
import qualified Data.Text as T
import Hasql.TH
import Data.Default.Class
import GHC.Generics (Generic)
import Data.Default.Class.Extended ()
import Data.Text.Extended ()
import TH.Mk
import Control.Lens
import Data.Maybe
import Database.Transaction (ParamsShow (..))
import qualified Hasql.Statement as HS
import Test.QuickCheck.Extended ()
import Data.Aeson.Types (Value (String), FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson (withText)
import Data.Bifunctor (second, first)
import qualified Data.Vector as V
import Data.String.Conv (toS)
import Data.Tuple.Extended (snocT, consT)
import Data.Aeson.Generic.DerivingVia
import Data.Swagger.Schema.Extended (deriveToSchemaConstructorTag)
import Data.Char (toLower)

data Status = 
     Draft | 
     Submit |
     PickedByTelnyx |
     PhonesPickedForCallByTelnyx |
     ProcessedByTelnyx |
     TranscriptionsDoneOpenAI |
     SentimentalAnalysisDoneOpenAI |
     SurveyProcessed | 
     Fail |
     TelnyxAppFailure T.Text |
     InsufficientFunds Status
  deriving Generic

instance Show Status where
    show Draft = "draft"
    show Submit = "submit"
    show PickedByTelnyx = "telnyx app is created"
    show ProcessedByTelnyx = "processed by telnyx"
    show PhonesPickedForCallByTelnyx = "phones picked for calling"
    show TranscriptionsDoneOpenAI = "transcriptions are finished"
    show SentimentalAnalysisDoneOpenAI = "sentimental analysis is finished"
    show SurveyProcessed = "processed"
    show Fail = "fail"
    show (InsufficientFunds s) = "insufficient_funds:" <> show s
    show (TelnyxAppFailure s) = "telnyx:" <> toS s

instance ParamsShow Status where
    render = show

instance Default Status where
    def = Draft


instance FromJSON Status where
  parseJSON = 
    withText "Status" $ \case
      "draft" -> pure Draft
      "submit" -> pure Submit
      "telnyx app is created" -> pure PickedByTelnyx
      "processed by telnyx" -> pure ProcessedByTelnyx
      "transcriptions are finished" -> pure TranscriptionsDoneOpenAI
      "sentimental analysis is finished" -> pure SentimentalAnalysisDoneOpenAI
      "processed" -> pure SurveyProcessed
      "fail" -> pure Fail
      "phones picked for calling" -> pure PhonesPickedForCallByTelnyx
      str -> 
        if T.isPrefixOf "telnyx:" str
          then pure $ maybe undefined TelnyxAppFailure $ T.stripPrefix "telnyx:" str
        else if T.isPrefixOf "insufficient_funds:" str
          then 
            let s = fromMaybe undefined $ T.stripPrefix "insufficient_funds:" str  
            in fmap InsufficientFunds $ parseJSON (String s)
        else fail $ toS str <> " doesn't fall into Status type"

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

data AssessmentScore = Yn | ScaleOfTen
    deriving stock (Show, Eq, Generic, Read)
    deriving
    (ToJSON, FromJSON)
    via WithOptions
        '[SumEnc UntaggedVal, ConstructorTagModifier '[UserDefined FirstLetterToLower]]
        AssessmentScore

deriveToSchemaConstructorTag ''AssessmentScore [| \(head:tail) -> toLower head : tail |]

instance Default AssessmentScore where
    def = Yn

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
 

data InsertSurveyKeys = 
     InsertSurveyKeys 
     { insertSurveyKeysSurvey :: Int64, 
       insertSurveyKeysDraft :: Int64 
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor InsertSurveyKeys)]]
          InsertSurveyKeys

insert :: HS.Statement Survey (Maybe Value)
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
        (user_id, survey_status, latitude, longitude, category, survey_type)
        select
          p.id :: int8,
          $3 :: text, 
          $4 :: float8, 
          $5 :: float8,
          $6 :: text,
          $7 :: text
        from auth.user as u 
        inner join customer.profile as p
        on u.id = p.user_id
        where u.id = $1 :: int8 
        and u.is_email_confirmed
        returning id :: int8 as ident),
      draft as (
        insert into customer.survey_draft 
        (survey, survey_id) 
        select $2 :: text, ident 
        from survey 
        returning id :: int8 as ident),
      files as (
        insert into customer.survey_files 
        (survey_id, phones_id)
        select ident, $8 :: int8 
        from survey)
    select
      jsonb_build_object(
        'survey', ident,
        'draft', (select ident from draft)) :: jsonb    
    from survey|]

data InsertDraft = InsertDraft { insertDraftIdent :: Int64, insertDraftSurveyType :: T.Text }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor InsertDraft)]]
          InsertDraft

insertDraft :: HS.Statement (Int64, Int64, T.Text) (Maybe Value)
insertDraft =
  lmap (snocT (toS (show Draft)))
  [maybeStatement|
    with survey as (
      select s.id, s.survey_type
       from auth.user as u
       inner join customer.profile as p
       on u.id = p.user_id
       inner join customer.survey as s
       on s.user_id = p.id
       where u.id = $1 :: int8 
       and s.id = $2 :: int8
       and survey_status = $4 :: text)
    insert into customer.survey_draft
    (survey, survey_id) 
    values ($3 :: text, (select id from survey))
    returning 
      jsonb_build_object(
        'ident', id :: int8,
        'survey_type', (select survey_type from survey)
        ) :: jsonb|]

submit :: HS.Statement (Int64, Int64) Bool
submit = 
  dimap (snocT (toS (show Submit))) (> 0)
  [rowsAffectedStatement|
    update customer.survey
    set survey_status = $3 :: text
    where id = 
      (select s.id
       from auth.user as u
       inner join customer.profile as p
       on u.id = p.user_id
       inner join customer.survey as s
       on s.user_id = p.id
       inner join customer.survey_draft as sd
       on s.id = sd.survey_id
       inner join customer.survey_bark as sb
       on sd.id = sb.survey_draft_id
       where u.id = $1 :: int8 and s.id = $2 :: int8 and sb.voice_id is not null
       order by sd.id desc limit 1)|]

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
     , barkSurveyDraftId :: Int64
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
         on conflict on constraint bark__bark_ident_uq do
         update set
           bark_status = excluded.bark_status,
           bark_ident = foreign_api.bark.bark_ident 
           returning id :: int8 as ident)
      insert into customer.survey_bark
      (survey_draft_id, bark_id)
      select $4 :: int8, ident :: int8 from bark|]

updateBark :: HS.Statement (T.Text, BarkStatus) ()
updateBark = 
  lmap (second (T.pack . show)) $ 
  [resultlessStatement|
    update foreign_api.bark 
    set bark_status = $2 :: text, modified = now() 
    where bark_ident = $1 :: text|]

insertVoiceBark :: HS.Statement (T.Text, BarkStatus, Int64, Double) ()
insertVoiceBark =
  lmap (\x -> x & _2 %~ (T.pack . show)) $ 
  [resultlessStatement|
    with
      bark as 
      (update foreign_api.bark
       set bark_status = $2 :: text, 
           modified = now(),
           duration = trunc(cast ($4 :: float8 as decimal(10, 2)), 2)
       where bark_ident = $1 :: text
       returning id :: int8 as ident)
      update customer.survey_bark
       set voice_id = $3 :: int8
       where bark_id = (select ident from bark)|]

getHistory :: HS.Statement (Int64, Int32) (Maybe ([Value], Int32))
getHistory =
  rmap (fmap (first V.toList)) $
  [maybeStatement|
    with tbl as 
      (select
         t.*,
         s.created,
         sbf.id as voice_ident,
         (select survey from customer.survey_draft where id = t.draft_ident) as title
       from (
         select
           s.id :: int8 as survey_ident,
           f.id :: int8 as report_ident,
           s.survey_status :: text as status,
           max(sd.id) as draft_ident
         from auth.user as u
         inner join customer.profile as p
         on u.id = p.user_id
         inner join customer.survey as s
         on p.id = s.user_id 
         inner join customer.survey_draft as sd
         on sd.survey_id = s.id
         left join customer.survey_files as sf
         on s.id = sf.survey_id
         left join storage.file as f
         on sf.report_id = f.id
         where p.user_id = $1 :: int8
         group by s.id, f.id, s.survey_status
       ) as t
       inner join customer.survey as s
       on t.survey_ident = s.id
       left join (
         select sb.survey_draft_id, f.id
         from customer.survey_bark as sb
         left join storage.file as f
         on sb.voice_id = f.id 
       ) as sbf
       on sbf.survey_draft_id = t.draft_ident
       order by s.created desc),
    total as (select count(*) from tbl),
    history as (select * from tbl offset (($2 :: int4 - 1) * 10) limit 10)
    select 
      array_agg(
        jsonb_build_object(
          'surveyident', survey_ident,
          'reportident', report_ident, 
          'name', title,
          'timestamp', created,
          'status', status,
          'bark', 
            case
              when voice_ident is not null
              then jsonb_build_object('voice', voice_ident)
              else null
            end        
        )) :: jsonb[],
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

insertPhones :: HS.Statement (Int64, V.Vector (T.Text, Bool)) Bool
insertPhones =
  lmap (\(x, y) -> snocT (toS (show Fail)) $ consT x $ V.unzip y) $
  [singletonStatement|
    with phones as (
      insert into customer.survey_phones
      (survey_id, phone, is_valid_number)
      select $1 :: int8, phone :: text, is_valid_number :: bool
      from unnest($2 :: text[], $3 :: bool[]) 
        x(phone, is_valid_number)
      returning is_valid_number as valid)
    update customer.survey
    set survey_status = 
         case
           when (select bool_or(valid) from phones)
           then survey_status
           else $4 :: text
         end
    where id = $1 :: int8
    returning (select bool_or(valid) from phones) :: bool|]

getVoiceObject :: HS.Statement T.Text (Maybe (T.Text, T.Text, T.Text, [T.Text]))
getVoiceObject = 
  rmap (fmap (\x -> x & _4 %~ V.toList)) $
  [maybeStatement|
    select
      f.hash :: text,
      f.bucket :: text,
      sd.survey :: text,
      array(select trim(both '"' from cast(el as text)) 
            from json_array_elements(f.exts) as el) :: text[]
    from foreign_api.bark as b
    inner join customer.survey_bark as sb
    on b.id = sb.bark_id
    inner join customer.survey_draft as sd
    on sb.survey_draft_id = sd.id
    inner join customer.survey as s
    on s.id = sd.survey_id
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
    inner join customer.survey_draft as sd
    on cs.id = sd.survey_id
    left join customer.survey_bark as sb
    on sd.id = sb.survey_draft_id
    inner join foreign_api.bark as b
    on b.id = sb.bark_id
    where b.bark_ident = $1 :: text|]

getSurveyForTelnyxApp :: HS.Statement () [(Int64, T.Text)]
getSurveyForTelnyxApp = 
  dimap (const (toS (show Submit))) V.toList $ 
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
  lmap (snocT (toS (show PickedByTelnyx)))
  [resultlessStatement|
    with 
      survey as (
        update customer.survey
        set survey_status = $4 :: text
        where id = $1 :: int8
        returning id :: int8 as ident)
    insert into foreign_api.telnyx_app 
    (survey_id, telnyx_ident, application_name)
    select ident, $3 :: text, $2 :: text from survey|]

failTelnyxApp :: HS.Statement [(Int64, T.Text)] ()
failTelnyxApp = 
  lmap (V.unzip . V.fromList . map (second (toS . show . TelnyxAppFailure)))
  [resultlessStatement|
    update customer.survey
    set survey_status = failure
    from unnest($1 :: int8[], $2 :: text[]) 
      as x(ident, failure)
    where ident = id|]

data PhoneToCall = PhoneToCall { phoneToCallIdent :: Int64, phoneToCallPhone :: T.Text }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor PhoneToCall)]]
          PhoneToCall

getPhonesToCall :: HS.Statement () [(Int64, T.Text, T.Text, [Value])]
getPhonesToCall =
  dimap 
    (const (toS (show PickedByTelnyx))) 
    (V.toList . fmap (\x -> x & _4 %~ V.toList)) $
  [vectorStatement|
     with survey as 
       (select
          s.id as survey_ident,
          s.survey_status,
          max(sd.id) as survey_draft_ident
        from customer.survey as s
        inner join customer.survey_draft as sd
        on sd.survey_id = s.id
        group by s.id, s.survey_status)
     select
       t.id :: int8,
       t.telnyx_ident :: text,
       vsl.share_link_url :: text,
       array_agg(jsonb_build_object( 
        'phone', sp.phone,
        'ident', sp.id)) :: jsonb[]
     from (
       select
         b.bark_id as bark_ident,
         tmp.survey_ident,
         tmp.survey_status
       from survey as tmp
       inner join customer.survey_bark as b
       on tmp.survey_draft_ident = b.survey_draft_id) as tbl
     inner join customer.voice_share_link as vsl
     on tbl.bark_ident = vsl.bark_id
     inner join customer.survey_phones as sp
     on tbl.survey_ident = sp.survey_id
     inner join foreign_api.telnyx_app as t
     on t.survey_id = tbl.survey_ident
     where tbl.survey_status = $1 :: text and sp.is_valid_number
     group by tbl.survey_ident, t.id, t.telnyx_ident, vsl.share_link_url|]

data CallStatus = Invalid | CallMade | Hangup | Answered | Recorded
     deriving Generic
     deriving Show

instance Default CallStatus where
    def = Hangup

mkArbitrary ''CallStatus

instance ParamsShow CallStatus where
    render = show

insertAppCall :: HS.Statement [(Int64, T.Text, T.Text)] ()
insertAppCall = 
  lmap (snocT (toS (show PhonesPickedForCallByTelnyx)) . V.unzip4 . V.fromList . map (snocT (toS (show CallMade)))) $ 
  [resultlessStatement|
     with call as (  
       insert into customer.call_telnyx_app
       (phone_id, call_leg_id, call_control_id, call_status)
       select
         ident,
         call_leg_id_value, 
         call_control_id_value,
         call_made_status
       from unnest($1 :: int8[], $2 :: text[], $3 :: text[], $4 :: text[])
         as x(ident, call_leg_id_value, call_control_id_value, call_made_status)
       on conflict (call_leg_id) do nothing
       returning phone_id :: int8 as ident)
     update customer.survey 
     set survey_status = $5 :: text
     where id =
        (select 
           distinct s.id
         from customer.survey as s
         inner join customer.survey_phones as p
         on s.id = p.survey_id
         where p.id in (select * from call))|]


getVoiceLinkByCallLegId :: HS.Statement T.Text (Maybe T.Text)
getVoiceLinkByCallLegId = 
  [maybeStatement|
    select
      vsl.share_link_url :: text
    from customer.call_telnyx_app as cta
    inner join customer.survey_phones as sp
    on cta.phone_id = sp.id
    inner join customer.survey_draft as sd
    on sd.survey_id = sp.survey_id
    inner join customer.survey_bark as sb
    on sd.id = sb.survey_draft_id
    inner join foreign_api.bark as b
    on sb.bark_id = b.id
    inner join customer.voice_share_link as vsl
    on vsl.bark_id = b.id
    where cta.call_leg_id = $1 :: text
    order by sd.id desc limit 1|]

updateAppCall :: HS.Statement (T.Text, T.Text) ()
updateAppCall = 
  [resultlessStatement|
     update customer.call_telnyx_app
     set call_status = $1 :: text
     where call_leg_id = $2 :: text|]

insertHangupCall :: HS.Statement (T.Text, T.Text) ()
insertHangupCall =
  lmap (snocT (toS (show Hangup)))
  [resultlessStatement|
    update customer.call_telnyx_app
    set call_status = $3 :: text,
        call_hangup_cause = $2 :: text 
    where call_leg_id = $1 :: text 
    and call_hangup_cause is null|]

checkAfterWebhook ::  HS.Statement T.Text ()
checkAfterWebhook =
  lmap ((,toS (show ProcessedByTelnyx), toS (show Fail)))
  [resultlessStatement|
    with survey as (
      select 
        s.id,
        count(*) as all_calls,
        count(cta.invalid) +
        count(cta.call_hangup_cause) as error_calls,
        count(cta.voice_id) as ok_calls
      from customer.survey as s
      inner join foreign_api.telnyx_app as ta
      on s.id = ta.survey_id
      inner join customer.survey_phones as sp
      on sp.survey_id = s.id
      inner join customer.call_telnyx_app as cta
      on cta.phone_id = sp.id
      where ta.telnyx_ident = $1 :: text
      group by s.id)
    update customer.survey
    set survey_status = 
          case
            when (select all_calls = error_calls from survey) then $3 :: text 
            when (select all_calls = error_calls + ok_calls from survey) then $2 :: text
            else survey_status
          end  
    where id = (select id from survey)|]

getUserByAppIdent :: HS.Statement T.Text (Maybe (Int64, T.Text))
getUserByAppIdent = 
  [maybeStatement|
     select
       distinct on (u.id, s.id)
       u.id :: int8,
       ('user' || cast(u.id as text) || '_' || 'survey' || cast(s.id as text)) :: text
     from auth.user as u
     inner join customer.survey as s
     on u.id = s.user_id
     inner join foreign_api.telnyx_app as ta
     on ta.survey_id = s.id
     where ta.telnyx_ident = $1 :: text
     group by u.id, s.id|]

insertVoiceTelnyx :: HS.Statement (T.Text, Int64) ()
insertVoiceTelnyx = 
  lmap (snocT (toS (show Recorded)))
  [resultlessStatement|
     update customer.call_telnyx_app
     set call_status = $3 :: text,
         voice_id = $2 :: int8 
     where call_leg_id = $1 :: text 
     and call_hangup_cause is null|]

data OpenAITranscription = 
     OpenAITranscription 
     { openAITranscriptionPhoneIdent :: Int64, 
       openAITranscriptionVoiceBucket :: T.Text,
       openAITranscriptionVoiceHash :: T.Text,
       openAITranscriptionVoiceTitle :: T.Text,
       openAITranscriptionVoiceExts :: [T.Text]
     } 
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor OpenAITranscription)]]
          OpenAITranscription

getSurveysForTranscription :: HS.Statement () [(Int64, [Value])]
getSurveysForTranscription = 
  dimap (const (toS (show ProcessedByTelnyx))) (V.toList . fmap (second V.toList)) $
  [vectorStatement|
    select
      s.id :: int8,
      array_agg(jsonb_build_object(
        'phone_ident', sp.id :: int8,
        'voice_bucket', f.bucket,
        'voice_hash', f.hash,
        'voice_title', f.title,
        'voice_exts',
         array(
          select 
            trim(both '"' from cast(el as text)) 
          from json_array_elements(f.exts) as el))
      ) :: jsonb[]
    from customer.survey as s
    inner join customer.survey_phones as sp
    on s.id = sp.survey_id
    inner join customer.call_telnyx_app as cta
    on sp.id = cta.phone_id
    inner join storage.file as f
    on cta.voice_id = f.id
    left join customer.phone_transcription as pt
    on pt.phone_id = sp.id
    where s.survey_status = $1 :: text 
    and (pt.transcription is null or pt.error is not null) and not pt.is_stuck
    group by s.id|]

data OpenAISA = 
     OpenAISA
     { openAISAPhoneIdent :: Int64,
       openAISAText :: T.Text
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor OpenAISA)]]
          OpenAISA     

getSurveysForSA :: HS.Statement () [(Int64, [Value])]
getSurveysForSA = 
   dimap (const (toS (show TranscriptionsDoneOpenAI))) (V.toList . fmap (second V.toList)) $
   [vectorStatement|
     select
       s.id :: int8,
       array_agg(jsonb_build_object(
         'phone_ident', pt.phone_id, 
         'text', pt.transcription)) :: jsonb[] 
     from customer.survey as s
     inner join customer.phone_transcription as pt
     on s.id = pt.survey_id
     where s.survey_status = $1 :: text 
     and pt.transcription is not null
     group by s.id|]


data TranscriptionText = 
     TranscriptionText 
     { transcriptionTextResult :: Maybe T.Text,
       transcriptionTextError :: Maybe T.Text
     }
     deriving stock (Generic)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor TranscriptionText)]]
          TranscriptionText

instance ParamsShow TranscriptionText where
  render = show . toJSON  

mkArbitrary ''TranscriptionText

mkTranscriptionOk :: T.Text -> T.Text -> TranscriptionText
mkTranscriptionOk prefix ok = TranscriptionText (Just (prefix <> ": " <> ok)) Nothing

mkTranscriptionFailure :: T.Text -> TranscriptionText
mkTranscriptionFailure err = TranscriptionText Nothing (Just err)

insertTranscription :: HS.Statement (Int64, [(Int64, TranscriptionText)]) ()
insertTranscription = 
  lmap (\(x, y) ->
      consT x $ 
        V.unzip $ 
          V.fromList $ 
            map (second toJSON) y) $
  [resultlessStatement|
    insert into customer.phone_transcription
    (survey_id, phone_id, transcription, error, attempts)
    select 
      $1 :: int8, 
      tbl.phone_id,
      result ->> 'result',
      result ->> 'error',
      tbl.attempts +
      case when result ->> 'error' is not null then 1 else 0 end 
    from (
     select
       x.phone_id,
       x.result as result,
       coalesce(pt.attempts, 0) as attempts
     from unnest($2 :: int8[], $3 :: jsonb[])
     as x(phone_id, result)
     left join customer.phone_transcription as pt
     on pt.phone_id = x.phone_id) as tbl
    on conflict on constraint phone_openai__phone_survey_uq
    do update set
    transcription = 
      coalesce(phone_transcription.transcription, excluded.transcription),
    error = 
      case 
        when excluded.transcription is not null then null
        when phone_transcription.attempts + 1 > 5 then null
        else excluded.error
      end,
    attempts = 
      case 
        when excluded.error is not null then 
          phone_transcription.attempts + 1 
        else phone_transcription.attempts
      end,
    is_stuck = phone_transcription.attempts + 1 > 5|]

checkAfterTranscription :: HS.Statement Int64 ()
checkAfterTranscription = 
  lmap (, toS (show TranscriptionsDoneOpenAI))
  [resultlessStatement|
    update customer.survey 
    set survey_status = $2 :: text
    where id = $1 :: int8
     and (
       select
         (count(pt.error) = 0) :: bool
       from customer.phone_transcription as pt
       where pt.survey_id = $1 :: int8)|]

insertSA :: HS.Statement (Int64, [(Int64, T.Text)]) ()
insertSA = 
  lmap (\(x, y) -> snocT (toS (show SentimentalAnalysisDoneOpenAI)) $ consT x $ V.unzip $ V.fromList y) $
  [resultlessStatement|
    with 
      analysis as (  
        insert into customer.phone_sentiment_analysis
        (survey_id, phone_id, result)
        select $1 :: int8, phone_id, res
        from unnest( $2 :: int8[], $3 :: text[]) 
        as x(phone_id, res)
        returning 1 :: int4)
    update customer.survey 
    set survey_status = $4 :: text
    where id = $1 :: int8 and (select (count(*) > 0) :: bool from analysis)|]

data SurveyForReportItem =
     SurveyForReportItem
     { surveyForReportItemPhone :: T.Text,
       surveyForReportItemResult :: T.Text
     }
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor SurveyForReportItem)]]
          SurveyForReportItem   

getSurveyForReport :: HS.Statement () [(Int64, Int64, [Value])]
getSurveyForReport =
  dimap (const (toS (show Fail), toS (show SentimentalAnalysisDoneOpenAI))) (V.toList . fmap (second V.toList)) $
  [vectorStatement|
    with 
      failure_report as (
        select
          distinct on (s.id, u.id, sp.id)
          s.id :: int8 as survey_id,
          u.id :: int8 as user_id,
          sp.id as phone_id,
          sp.phone,
          case 
            when not sp.is_valid_number then 
              'phone is invalid'
            when cta.invalid is not null then 
              'phone is invalid'
            else trim(both '"' from cta.call_hangup_cause)
          end as result
        from auth.user as u
        inner join customer.survey as s
        on u.id = s.user_id
        inner join customer.survey_phones as sp
        on s.id = sp.survey_id
        left join customer.call_telnyx_app as cta 
        on cta.phone_id= sp.id
        where s.survey_status = $1 :: text and
              (select report_id 
               from customer.survey_files 
               where survey_id = s.id) is null),
      success_report as (
        select
          distinct on (s.id, u.id, sp.id)
          s.id :: int8 as survey_id,
          u.id :: int8 as user_id,
          sp.id as phone_id,
          sp.phone,
          case 
            when psa.result is not null then 
              psa.result
            when cta.invalid is not null then 
              'phone is invalid'
            when cta.call_hangup_cause is not null then 
              trim(both '"' from cta.call_hangup_cause)
            when pt.is_stuck then 
              'transcription is stuck'
            else 'sentiment analysis error'
          end as result
        from auth.user as u
        inner join customer.survey as s
        on u.id = s.user_id
        inner join customer.survey_phones as sp
        on s.id = sp.survey_id
        inner join customer.call_telnyx_app as cta
        on cta.phone_id = sp.id
        left join customer.phone_transcription as pt
        on pt.phone_id = sp.id
        left join customer.phone_sentiment_analysis as psa
        on psa.phone_id = sp.id
        where s.survey_status = $2 :: text and
              (select report_id 
               from customer.survey_files
               where survey_id = s.id) is null)
     select 
       survey_id :: int8, 
       user_id :: int8, 
       array_agg(jsonb_build_object(
         'phone', phone,
         'result', result
       )) :: jsonb[] 
     from failure_report
     group by survey_id, user_id
     union 
     select 
       survey_id :: int8, 
       user_id :: int8, 
       array_agg(jsonb_build_object(
         'phone', phone,
         'result', result
       )) :: jsonb[]
     from success_report
     group by survey_id, user_id|]

saveReport :: HS.Statement (Int64, Int64) ()
saveReport =
  lmap (snocT (toS (show SurveyProcessed)))
  [resultlessStatement|
    with report as (
      update customer.survey_files 
      set report_id = $2 :: int8
      where survey_id = $1 :: int8
      returning 1 :: int4)
    update customer.survey
    set survey_status = $3 :: text,
        processed = now()
    where id = $1 :: int8 and (select (count(*) > 0) :: bool from report)|]

invalidatePhones :: HS.Statement [(Int64, T.Text)] (Maybe Int64)
invalidatePhones =
  lmap (V.unzip3 . V.fromList . map (snocT (toS (show Invalid))))
  [maybeStatement|
      with invalid as
        (insert into customer.call_telnyx_app
         (phone_id, invalid, call_status)
         select ident, reason, invalid_status
         from unnest($1 :: int8[], $2 :: text[], $3 :: text[]) 
           as phones(ident, reason, invalid_status))
      select
        distinct s.id :: int8
      from customer.survey as s
      inner join customer.survey_phones as sp
      on s.id = sp.survey_id
      where sp.id = any($1 :: int8[])|]

checkAfterInvalidate :: HS.Statement Int64 ()
checkAfterInvalidate =
  lmap ((,toS (show PhonesPickedForCallByTelnyx), toS (show Fail)))
  [resultlessStatement|
    with survey as (
       select
         s.id,
         (count(cta.invalid) = count(*)) :: bool as all_invalid
       from customer.survey as s
       inner join customer.survey_phones as cp
       on s.id = cp.survey_id
       inner join customer.call_telnyx_app as cta
       on cp.id = cta.phone_id
       where s.id = $1 :: int8
       group by s.id)
    update customer.survey 
    set survey_status = 
         case 
           when (select all_invalid from survey) then $3 :: text
           else $2 :: text
         end
    where id = (select id from survey)|]

insertStat :: HS.Statement [Int64] ()
insertStat = 
  lmap V.fromList
  [resultlessStatement|
    insert into public.phone_transcription_result
    (phone, transcription, result)
    select
     sp.phone :: text, 
     pt.transcription :: text, 
     psa.result :: text
    from unnest($1 :: int8[]) as s(ident)
    inner join customer.survey_phones as sp
    on s.ident = sp.survey_id
    inner join customer.phone_transcription as pt
    on sp.id = pt.phone_id
    inner join customer.phone_sentiment_analysis as psa
    on sp.id = psa.phone_id|]

data DailyPhoneStat = 
     DailyPhoneStat 
     { dailyPhoneStatPhone :: T.Text,
       dailyPhoneStatTranscription :: T.Text, 
       dailyPhoneStatResult :: T.Text
     }
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[CamelTo2 "_", UserDefined (StripConstructor DailyPhoneStat)]]
          DailyPhoneStat   

getDailyPhoneStat :: HS.Statement () [Value]
getDailyPhoneStat =
  rmap V.toList
  [vectorStatement|
    select
      jsonb_build_object(
        'phone', phone,
        'transcription', transcription,
        'result', result) :: jsonb
    from public.phone_transcription_result
    where cast(created_at as date) = cast(now() as date) - 1|]

setInsufficientFund :: HS.Statement (Int64, Status) ()
setInsufficientFund = lmap (second (toS . show . InsufficientFunds)) $ [resultlessStatement|update customer.survey set survey_status = $2 :: text where id = $1 :: int8|]