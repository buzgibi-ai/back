{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Buzgibi.Statement.User.Notification (Type (..), Level (..), GetNotification (..), get, insert) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Time.Clock (UTCTime)
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Lens hiding (Level)
import TH.Mk
import Data.Tuple.Extended (app2, app3)
import Test.QuickCheck.Extended ()
import Data.Maybe (fromMaybe)
import Data.Swagger.Schema.Extended 
       (deriveToSchemaConstructorTag, 
        deriveToSchemaFieldLabelModifier, 
        modify)
import Data.Proxy (Proxy (..))
import Data.Char (toLower)

data Type =
      -- "2023-08-11T08:29:01.597416Z" 
      Until UTCTime |
      -- "true | false"
      Perpetual Bool
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[SumEnc UntaggedVal, ConstructorTagModifier '[CamelTo2 "_"]]
          Type

mkArbitrary ''Type

data Level = Info | Warning | Alert
    deriving stock (Generic, Show)
    deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[SumEnc UntaggedVal, ConstructorTagModifier '[UserDefined ToLower]]
          Level

mkArbitrary ''Level

deriveToSchemaConstructorTag ''Level [|\s -> toLower (head s) : tail s |]

data GetNotification = 
     GetNotification 
     { getNotificationText :: T.Text, 
       getNotificationLevel :: Level 
     } 
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor GetNotification)]]
          GetNotification

mkArbitrary ''GetNotification

deriveToSchemaFieldLabelModifier ''GetNotification [|modify (Proxy @GetNotification)|]

get :: HS.Statement Int64 [Value]
get =
  rmap V.toList
  [vectorStatement|
    select
      jsonb_build_object(
        'text', tbl.text,
        'level', tbl.level
      ) :: jsonb
    from (
      select
        pn.text as text,
        pn.level as level,
        case
          when jsonb_typeof(un.type) = 'string' then
            cast((cast(un.type as text)) as timestamp) > now()
          when jsonb_typeof(un.type) = 'boolean' then
            cast(un.type as boolean)
        end as is_shown
      from auth.user as u
      inner join customer.profile as p
      on u.id = p.user_id
      inner join customer.user_notification as un
      on p.id = un.user_id
      inner join public.notification as pn
      on un.notification_id = pn.id and un.user_id = $1 :: int8) 
      as tbl
    where tbl.is_shown|]

data CreateNotification = 
     CreateNotification 
     { createNotificationText :: T.Text, 
       createNotificationType :: Type, 
       createNotificationAudience :: [Int64]
     }
     deriving Generic


mkEncoder ''CreateNotification
mkArbitrary ''CreateNotification

encodeCreateNotification = fromMaybe (error "cannot encode CreateNotification") . mkEncoderCreateNotification

insert :: HS.Statement CreateNotification ()
insert = 
  lmap (app3 V.fromList . app2 toJSON . encodeCreateNotification)
  [resultlessStatement|
    with notification as 
    (insert into public.notification (text) 
     values ($1 :: text) returning id :: int8)
    insert into customer.user_notification
    (user_id, notification_id, type)
    select user_ident, (select id from notification), $2 :: jsonb
    from unnest($3 :: int8[]) as user_ident|]