module Buzgibi.Api.CallApi.Instance () where

import Buzgibi.Api.CallApi (IsApi (..))
import Buzgibi.EnvKeys (Telnyx (..))

instance IsApi Telnyx where
  getUrl = telnyxUrl
  getKey = telnyxKey