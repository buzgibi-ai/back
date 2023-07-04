module Buzgibi.Api.Controller.Utils (withError) where

import Buzgibi.Transport.Response
import Control.Lens.Iso.Extended
import Control.Lens

withError :: Show e => Either e a -> (a -> b) -> Response b
withError (Left e) _ = Error $ asError (show e^.stext)
withError (Right x) ok = Ok $ ok x