{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Buzgibi.Statement.User.Auth (insert, insertToken, logout) where

import qualified Data.Text as T
import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import Data.Coerce

insert :: HS.Statement (T.Text, T.Text, T.Text) Bool
insert =
    dimap coerce (> 0) $
    [rowsAffectedStatement|
       with new_user as (
         insert into auth.user
         (email, pass)
         values ($1 :: text, crypt($2 :: text, gen_salt('md5')))
         on conflict on constraint email__uk do nothing
         returning id :: int8)
       insert into auth.jwt 
       (user_id, jwt) 
       select id :: int8, $3 :: text from new_user|]

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

logout :: HS.Statement T.Text Bool
logout = dimap coerce (> 0) $ [rowsAffectedStatement| update auth.jwt set is_valid = false where jwt = $1 :: text and is_valid |]