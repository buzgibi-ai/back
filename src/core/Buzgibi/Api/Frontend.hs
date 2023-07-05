{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Frontend (FrontendApi (..)) where

import Buzgibi.Api.Controller.Frontend.GetCookies (Cookie)
import Buzgibi.Api.Controller.Frontend.GetMeta (Meta)
import Buzgibi.Api.Controller.Frontend.Init (Init)
import Buzgibi.Api.Controller.Frontend.Log (FrontendLogRequest)
import Buzgibi.Api.Controller.Frontend.Translate hiding (controller)
import Buzgibi.Transport.Model.User (AuthToken)
import Buzgibi.Transport.Response (Response)
import qualified Data.Text as T
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
          :> ReqBody '[JSON] AuthToken
          :> Post '[JSON] (Response Init),
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
          :> QueryParam' '[Optional, Strict] "page" T.Text
          :> Get '[JSON] (Response Meta)
  }
  deriving stock (Generic)
