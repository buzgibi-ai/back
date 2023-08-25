{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Buzgibi.Api.Http.Test (spec_api) where

import Data.Proxy
import Buzgibi.Api.File
import Buzgibi.Api.User (AuthApi, UserApi)
import Buzgibi.Api.Controller.User.Survey.Make (Survey, Location)
import Buzgibi.Api.Controller.User.GetHistory (History, Status, HistoryItem)
import Buzgibi.Transport.Model.User (Credentials, AuthToken)
import Buzgibi.Api.Foreign (SendGridApi, WebhookApi)
import Buzgibi.Api.Controller.SendGrid.SendMail (SendGridSendMailRequest)
import Buzgibi.Config (Email)
import Buzgibi.Api.Frontend (FrontendApi)
import Buzgibi.Api.Controller.Frontend.Log (FrontendLogRequest)
import Buzgibi.Api.Controller.Frontend.Init (Init, Env, JWTStatus)
import Buzgibi.Transport.Model.Translation
import Buzgibi.Api.Controller.Frontend.GetCookies (Cookie, SameSiteOption)
import Buzgibi.Api.Controller.Frontend.GetMeta (Meta)
import Buzgibi.Api.Controller.User.Survey.Edit (EditSurvey)
import Buzgibi.Api.Controller.User.Survey.Submit (SubmitSurvey)
import Buzgibi.Api.Controller.Auth.Password.MakeLink (ResetPasswordLink)
import Buzgibi.Api.Controller.Auth.Password.Create (NewPassword) 

import Servant.API.Generic
import Servant.Swagger.Test
import Test.Hspec
import TH.Mk (mkArbitrary)
import Test.QuickCheck.Extended (Arbitrary (..))


instance (Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = Map <$> arbitrary <*> arbitrary

mkArbitrary ''AuthToken
mkArbitrary ''HistoryItem
mkArbitrary ''Status
mkArbitrary ''Location
mkArbitrary ''Survey
mkArbitrary ''History
mkArbitrary ''Credentials
mkArbitrary ''Email
mkArbitrary ''SendGridSendMailRequest
mkArbitrary ''FrontendLogRequest
mkArbitrary ''Env
mkArbitrary ''JWTStatus
mkArbitrary ''Init
mkArbitrary ''Page
mkArbitrary ''Menu
mkArbitrary ''Resource
mkArbitrary ''Endpoints
mkArbitrary ''Translation
mkArbitrary ''SameSiteOption
mkArbitrary ''Meta
mkArbitrary ''Cookie
mkArbitrary ''EditSurvey
mkArbitrary ''SubmitSurvey
mkArbitrary ''ResetPasswordLink
mkArbitrary ''NewPassword

spec_api :: Spec
spec_api =
  describe "Swagger spec for API" $ do
    context "ToJSON matches ToSchema (FileApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))
    context "ToJSON matches ToSchema (AuthApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy AuthApi))
    context "ToJSON matches ToSchema (UserApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy UserApi))
    context "ToJSON matches ToSchema (SendGridApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy SendGridApi))
    context "ToJSON matches ToSchema (WebhookApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy WebhookApi))
    context "ToJSON matches ToSchema (FrontendApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy FrontendApi)) 