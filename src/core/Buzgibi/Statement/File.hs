{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Buzgibi.Statement.File
  ( save,
    getMeta,
    getMetaForReport,
    Buzgibi.Statement.File.delete,
    getHashWithBucket,
    patch,
    NewFile (..)
  )
where

import Buzgibi.Transport.Id
import Buzgibi.Transport.Model.File
import Control.Foldl
import Control.Lens
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Statement as HS
import Hasql.TH
import TH.Mk
import GHC.Generics
import Database.Transaction (ParamsShow (..))
import Data.Maybe (fromMaybe)
import Data.Aeson (toJSON)

data NewFile = 
     NewFile 
     { newFileHash :: T.Text, 
       newFileName :: T.Text, 
       newFileMime :: T.Text, 
       newFileBucket :: T.Text, 
       newFileExts :: [T.Text]
     }
     deriving (Generic, Show)

mkEncoder ''NewFile
mkArbitrary ''NewFile

encodeNewFile = fromMaybe (error "cannot encode NewFile") . mkEncoderNewFile

instance ParamsShow NewFile where
  render = render . encodeNewFile

save :: HS.Statement [NewFile] [Id "file"]
save =
  lmap
    ( V.unzip5
        . V.fromList
        . Prelude.map
          (\x -> encodeNewFile x & _5 %~ toJSON)
    )
    $ statement
    $ premap (^. coerced) list
  where
    statement =
      [foldStatement|
        insert into storage.file
        (hash, title, mime, bucket, exts)
        select x.hash, x.title, x.mime, x.bucket, x.exts
        from unnest(
          $1 :: text[],
          $2 :: text[],
          $3 :: text[],
          $4 :: text[],
          $5 :: json[]) as
          x(hash, title, mime, bucket, exts)
          returning id :: int8|]

getMetaForReport :: HS.Statement (Id "user", Id "file") (Maybe (Hash, Name, Mime, Bucket, [T.Text]))
getMetaForReport =
  dimap (\x -> x & _1 %~ coerce & _2 %~ coerce) (fmap mkTpl) $
    [maybeStatement|
      select
        coalesce(report.hash, voice.hash) :: text as hash, 
        coalesce(report.title, voice.title) :: text as title,
        coalesce(report.mime, voice.mime) :: text as mime, 
        coalesce(report.bucket, voice.bucket) :: text as bucket,
        array(select trim(both '"' from cast(el as text)) 
            from json_array_elements(coalesce(report.exts, voice.exts)) as el) :: text[] as ext
      from auth.user as u      
      inner join customer.profile as p
      on u.id = p.user_id
      inner join customer.survey as ce
      on p.id = ce.user_id
      left join (
        select 
          sf.survey_id,
          f.hash :: text, 
          f.title :: text,
          f.mime :: text, 
          f.bucket :: text,
          f.id :: int8,
          f.exts
        from customer.survey_files as sf
        left join storage.file as f
        on sf.report_id = f.id
        where f.id = $2 :: int8) as report
      on report.survey_id = ce.id
      left join (
        select
          sd.survey_id,
          f.hash :: text, 
          f.title :: text,
          f.mime :: text, 
          f.bucket :: text,
          f.exts,
          f.id :: int8
        from customer.survey_draft as sd
        left join customer.survey_bark as sb
        on sd.id = sb.survey_draft_id
        left join storage.file as f
        on sb.voice_id = f.id
        where f.id = $2 :: int8
      ) as voice
      on voice.survey_id = ce.id
      where u.id = $1 :: int8 and (voice is not null or report is not null)|]
  where mkTpl x = x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce & _4 %~ coerce & _5 %~ V.toList @T.Text

getMeta :: HS.Statement (Id "file") (Maybe (Hash, Name, Mime, Bucket, [T.Text]))
getMeta =
  dimap coerce (fmap mkTpl) $
    [maybeStatement|
      select
       f.hash :: text, 
       f.title :: text,
       f.mime :: text, 
       f.bucket :: text,
       array(select trim(both '"' from cast(el as text)) 
            from json_array_elements(exts) as el) :: text[]
      from storage.file as f
      where f.id = $1 :: int8
            and not is_deleted|]
  where mkTpl x = x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce & _4 %~ coerce & _5 %~ V.toList @T.Text

delete :: HS.Statement (Id "file") Bool
delete =
  dimap coerce (> 0) $
    [rowsAffectedStatement|
    update storage.file
    set deleted = now(),
    is_deleted = true
    where id = $1 :: int8 and not is_deleted :: bool|]

getHashWithBucket :: HS.Statement (Id "file") (Maybe (Hash, Bucket))
getHashWithBucket =
  dimap (^. coerced) (fmap (\x -> x & _1 %~ coerce & _2 %~ coerce)) $
    [maybeStatement|
    select hash :: text, bucket :: text
    from storage.file
    where id = $1 :: int8|]

patch :: HS.Statement (Name, Mime, Hash) ()
patch =
  lmap (\x -> x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce) $
    [resultlessStatement|
    update storage.file set
    title = $1 :: text,
    mime = $2 :: text,
    modified = now()
    where hash = $3 :: text|]
