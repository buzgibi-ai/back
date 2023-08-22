{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Statement.User.Auth (insertUser, insertJwt, insertToken, logout, getUserIdByEmail, insertConfirmationLink) where

import Control.Lens
import Data.Coerce
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH

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

insertJwt :: HS.Statement (Int64, T.Text) Bool
insertJwt =
  dimap coerce (> 0) $
    [rowsAffectedStatement|
       insert into auth.jwt 
       (user_id, jwt) 
       values ($1 :: int8, $2 :: text)|]

getUserIdByEmail :: HS.Statement T.Text (Maybe Int64)
getUserIdByEmail = [maybeStatement|select id :: int8 from auth.user where email = $1 :: text|]

insertToken :: HS.Statement (T.Text, T.Text, T.Text) Bool
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
       insert into auth.jwt (user_id, jwt)
       select id :: int8, $3 :: text from user_ident 
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