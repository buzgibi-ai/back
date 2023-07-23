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
    getMetaForBark,
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
     deriving Generic

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

getMetaForBark :: HS.Statement (Id "user", Id "file") (Maybe (Hash, Name, Mime, Bucket, [T.Text]))
getMetaForBark =
  dimap (\x -> x & _1 %~ coerce & _2 %~ coerce) (fmap mkTpl) $
    [maybeStatement|
      select
       f.hash :: text, 
       f.title :: text,
       f.mime :: text, 
       f.bucket :: text,
       array(select trim(both '"' from cast(el as text)) 
            from json_array_elements(exts) as el) :: text[]
      from customer.profile as p
      inner join customer.survey as ce
      on p.id = ce.user_id
      inner join customer.survey_bark as eb
      on ce.id = eb.survey_id
      inner join storage.file as f
      on eb.voice_id = f.id
      where p.user_id = $1 :: int8 
            and f.id = $2 :: int8
            and not is_deleted|]
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
