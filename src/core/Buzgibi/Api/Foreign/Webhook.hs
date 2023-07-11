{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Foreign.Webhook (WebhookApi (..)) where

import Buzgibi.Transport.Payload (Payload)
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))


newtype WebhookApi route = WebhookApi
  { _webhookApiBark ::
      route
        :- "bark"
          :> ReqBody '[JSON] Payload
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
