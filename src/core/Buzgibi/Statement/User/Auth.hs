{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Statement.User.Auth 
       (insertUser, 
        insertJwt, 
        insertToken, 
        logout, 
        getUserIdByEmail, 
        insertConfirmationLink, 
        confirmEmail,
        resendLink,
        insertPasswordResetLink,
        insertNewPassword,
        checkToken
        ) where

import Control.Lens
import Data.Coerce
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson (Value)
import Data.Maybe (fromMaybe)

insertUser :: HS.Statement (T.Text, T.Text) (Maybe Int64)
insertUser =
  [maybeStatement|
    with 
      auth as 
      (insert into auth.user
       (email, pass)
       values ($1 :: text, crypt($2 :: text, gen_salt('md5')))
       on conflict on constraint email__uk do nothing
       returning id :: int8),
      profile as (insert into customer.profile (user_id) (select * from auth))
    select id :: int8 from auth|]

insertJwt :: HS.Statement (Int64, T.Text, Int64) Bool
insertJwt =
  dimap coerce (> 0) $
    [rowsAffectedStatement|
       insert into auth.jwt 
       (user_id, jwt, uuid_hash) 
       values ($1 :: int8, $2 :: text, $3 :: bigint)|]

getUserIdByEmail :: HS.Statement T.Text (Maybe Int64)
getUserIdByEmail = [maybeStatement|select id :: int8 from auth.user where email = $1 :: text|]

insertToken :: HS.Statement (T.Text, T.Text, T.Text, Int64) Bool
insertToken =
  dimap coerce (> 0) $
    [rowsAffectedStatement|
       with
         user_ident as (
           select 
           id,
           (pass = crypt($2 :: text, pass)) :: bool as is_pass_valid 
           from auth.user 
           where email = $1 :: text),
         invalidated_jwt as (
           update auth.jwt 
           set is_valid = false 
           where 
             is_valid and
             user_id = (select id from user_ident)
             and (select is_pass_valid from user_ident) is true )
       insert into auth.jwt (user_id, jwt, uuid_hash)
       select id :: int8, $3 :: text, $4 :: bigint from user_ident 
       where (select is_pass_valid from user_ident)|]

logout :: HS.Statement Int64 Bool
logout = dimap coerce (> 0) $ [rowsAffectedStatement| update auth.jwt set is_valid = false where user_id = $1 :: int8 and is_valid |]

insertConfirmationLink :: HS.Statement (Int64, T.Text) Bool
insertConfirmationLink =
  rmap (> 0)
  [rowsAffectedStatement|
    insert into auth.email_confirmation_link
    (user_id, link, valid_until)
    select id, $2 :: text, now() + interval '1 day'
    from auth.user 
    where id = $1 :: int8 and not is_email_confirmed|]

confirmEmail :: HS.Statement (Int64, T.Text) Bool
confirmEmail =
  rmap (> 0)
  [rowsAffectedStatement|
    update auth.user 
    set is_email_confirmed = true
    where id = $1 :: int8 and not is_email_confirmed and
    (select exists(select 1
     from auth.user as u 
     inner join auth.email_confirmation_link as l
     on u.id = l.user_id
     where l.link = $2 :: text 
     and u.id = $1 :: int8 
     and l.valid_until > now()
     order by l.id desc limit 1))|]

resendLink :: HS.Statement (Int64, T.Text) (Maybe Value)
resendLink =
  [maybeStatement|
    with 
      tmp as (
        select
          u.email, 
          l.created_at 
        from auth.user as u 
        inner join auth.email_confirmation_link as l
        on u.id = l.user_id 
        where u.id = $1 :: int8 and not is_email_confirmed
        order by l.id desc limit 1), 
      link as (
        insert into auth.email_confirmation_link
        (user_id, link, valid_until)
        select id, $2 :: text, now() + interval '1 day'
        from auth.user
        where id = $1 :: int8 
        and (select now() > created_at + interval '30 min' from tmp)
        returning (select to_jsonb(email) from tmp) as email) ,
      tm_left as (
        select to_jsonb(cast(extract(epoch from created_at + interval '30 min') - extract(epoch from now()) as int)) as tm
        from tmp 
        where (select * from link) is null)   
    select email :: jsonb from link union select tm :: jsonb from tm_left|]

insertPasswordResetLink :: HS.Statement (T.Text, T.Text) Value
insertPasswordResetLink =
  [singletonStatement|
     with 
       tmp as (
         select
           u.id as user_ident, 
           l.created_at, now()
         from auth.user as u 
         left join auth.password_reset_link as l
         on u.id = l.user_id
         where u.email = $1 :: text 
         and not coalesce(is_expended, false)
         order by l.id desc limit 1),
      link as (
        insert into auth.password_reset_link
        (user_id, link, valid_until)
        select user_ident, $2 :: text, now() + interval '5 min'
        from tmp
        where (select now() > coalesce(created_at, '1970-01-01'::timestamptz) + interval '30 min' from tmp)
        returning 1),
      tm_left as (
        select cast(extract(epoch from created_at + interval '30 min') - extract(epoch from now()) as int) as tm
        from tmp 
        where (select * from link) is null)
     select 
       coalesce(
	       (select to_jsonb(tm :: int8) :: jsonb from tm_left),
         (select to_jsonb('success' :: text) :: jsonb from link),
		     to_jsonb('user404' :: text) :: jsonb) :: jsonb|]

insertNewPassword :: HS.Statement (T.Text, T.Text) Bool
insertNewPassword =
  rmap (> 0)
  [rowsAffectedStatement|
    with link as (
      update auth.password_reset_link
      set is_expended = true
      where link = $2 :: text 
      and now() < valid_until 
      and is_expended is null
      returning user_id),
    jwt as (
      update auth.jwt 
      set is_valid = false 
      where user_id = (select user_id from link))  
    update auth.user
    set pass = crypt($1 :: text, gen_salt('md5')),
        modified = now()
    where id = (select user_id from link)|]

checkToken :: HS.Statement Int64 Bool
checkToken = rmap (fromMaybe False) $ [maybeStatement| select is_valid :: bool from auth.jwt where uuid_hash = $1 :: int8|]