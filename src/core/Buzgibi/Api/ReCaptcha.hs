{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.ReCaptcha (ReCaptchaApi (..)) where

import Buzgibi.Api.Controller.ReCaptcha.Verify (ReCaptcha, Token)
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.Swagger.Internal.Extended ()

newtype ReCaptchaApi route = ReCaptchaApi
  { _reCaptchaApiVerify ::
      route
        :- "verify"
          :> ReqBody '[JSON] Token
          :> Post '[JSON] (Response ReCaptcha)
  }
  deriving stock (Generic)
