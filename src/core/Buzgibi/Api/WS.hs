{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.WS (WSApi (..)) where

import Servant.API.Extended (type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

data WSApi route = WSApi
  { _wsApiUserHistory ::
      route
        :- "user"
          :> "survey"
          :> "history"
          :> "voice"
          :> WebSocketPending
  , _wsApiUserReport ::
      route
        :- "user"
          :> "survey"
          :> "history"
          :> "report"
          :> WebSocketPending
  }
  deriving stock (Generic)