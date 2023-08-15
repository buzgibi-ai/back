{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.WS (WSApi (..)) where

import Buzgibi.Api.Controller.WS.Survey.History (Resource)
import Servant.API.Extended (type (:>), Capture)
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

newtype WSApi route = WSApi
  { _wsApiUserHistory ::
      route
        :- "user"
          :> "survey"
          :> "history"
          :> Capture "resource" Resource
          :> WebSocketPending
  }
  deriving stock (Generic)