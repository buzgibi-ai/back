{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Foreign
  ( ForeignApi (..),
    module SendGrid,
    module WebhookApi
  )
where

import Buzgibi.Api.Foreign.Webhook as WebhookApi 
import Buzgibi.Api.Foreign.SendGrid as SendGrid
import Servant.API.Extended (AsApi, ToServant, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

data ForeignApi route = ForeignApi
  { _foreignApiSendGrid ::
      route
        :- "sendgrid"
          :> ToServant SendGridApi AsApi
  , _foreignApiWebhook ::
       route
        :- "webhook"
          :> ToServant WebhookApi AsApi
  }
  deriving stock (Generic)
