module Buzgibi.Api.CallApi.Instance () where

import Buzgibi.Api.CallApi (IsApi (..))
import Buzgibi.EnvKeys (Telnyx (..), OpenAI (..))

instance IsApi Telnyx where
  getUrl = telnyxUrl
  getKey = telnyxKey

instance IsApi OpenAI where
  getUrl = openAIUrl
  getKey = openAIKey