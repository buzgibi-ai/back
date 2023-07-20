{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Foreign.Webhook (WebhookApi (..), TelnyxApi (..)) where

import Buzgibi.Transport.Payload (Payload)
import Servant.API.Extended 
import Servant.API.Generic

data WebhookApi route = WebhookApi
  { _webhookApiBark ::
      route
        :- "bark"
          :> ReqBody '[JSON] Payload
          :> Post '[JSON] ()
  , _webhookApiTelnyx ::
      route
        :- "telnyx"
        :> ToServant TelnyxApi AsApi
  }
  deriving stock (Generic)

newtype TelnyxApi route = TelnyxApi 
  { _telnyxApiCatchApp ::
      route
        :- "app"
          :> ReqBody '[JSON] Payload
          :> Post '[JSON] ()
  }
  deriving stock (Generic)