{-# LANGUAGE NamedFieldPuns #-}

module Buzgibi.Api.Controller.Utils (withError, getContent, ContentError (..)) where

import Buzgibi.Transport.Response
import Control.Lens.Iso.Extended
import Control.Lens
import qualified GitHub as GitHub
import qualified Data.Text as T
import Data.Yaml (decodeEither', prettyPrintParseException)
import Data.ByteString.Base64 (decodeLenient)
import GitHub.Data.Content (contentFileContent)
import Data.Bifunctor (first)
import Data.Aeson (FromJSON)

withError :: Show e => Either e a -> (a -> b) -> Response b
withError (Left e) _ = Error $ asError (show e^.stext)
withError (Right x) ok = Ok $ ok x

data ContentError = Resource404 | Yaml T.Text

instance Show ContentError where
    show Resource404 = "resource not found"
    show (Yaml e) = "yaml cannot ve parsed"

getContent :: FromJSON a => Either e GitHub.Content -> Either ContentError a
getContent (Right (GitHub.ContentFile (GitHub.ContentFileData {contentFileContent}))) = 
  first (Yaml . T.pack . prettyPrintParseException) $ 
    decodeEither' $ 
      contentFileContent ^. textbs . to decodeLenient
getContent _ = Left Resource404