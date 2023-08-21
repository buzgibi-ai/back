{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Frontend (FrontendApi (..)) where

import Buzgibi.Transport.Model.Translation (Lang, Translation)
import Buzgibi.Api.Controller.Frontend.GetCookies (Cookie)
import Buzgibi.Api.Controller.Frontend.GetMeta (Meta, Page)
import Buzgibi.Api.Controller.Frontend.Init (Init)
import Buzgibi.Api.Controller.Frontend.Log (FrontendLogRequest)
import Buzgibi.Transport.Model.User (AuthToken)
import Buzgibi.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)

data FrontendApi route = FrontendApi
  { _frontendApiLog ::
      route
        :- "log"
          :> ReqBody '[JSON] FrontendLogRequest
          :> Put '[JSON] (Response ()),
    _frontendApiInit ::
      route
        :- "init"
          :> QueryParam' '[Optional] "token" AuthToken
          :> Get '[JSON] (Response Init),
    _frontendApiTranslate ::
      route
        :- "translate"
          :> Capture "lang" Lang
          :> Get '[JSON] (Response Translation),
    _frontendApiGetCookies ::
      route
        :- "cookies"
          :> Get '[JSON] (Response [Cookie]),
    _frontendApiGetMeta ::
      route
        :- "meta"
          :> QueryParam' '[Optional, Strict] "page" Page
          :> Get '[JSON] (Response Meta)
  }
  deriving stock (Generic)
