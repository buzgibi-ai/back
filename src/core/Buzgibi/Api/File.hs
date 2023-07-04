{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.File (FileApi (..)) where

import Buzgibi.Api.Controller.File.Download (Option)
import Buzgibi.Transport.Id
import Buzgibi.Transport.Response
import qualified Data.Text as T
import Servant.API.Extended
import Servant.API.Generic
import Servant.Multipart
import Servant.Multipart.File
import Servant.RawM

data FileApi route = FileApi
  { _fileApiUpload ::
      route
        :- Description "upload to server"
          :> Capture "bucket" T.Text
          :> MultipartForm Tmp Files
          :> Put '[JSON] (Response [Id "file"]),
    _fileApiPatch ::
      route
        :- Description "patch file by replacing new one"
          :> Capture "file_id" (Id "file")
          :> MultipartForm Tmp File
          :> Patch '[JSON] (Response ()),
    _fileApiDelete ::
      route
        :- Description "delete file"
          :> Capture "file_id" (Id "file")
          :> Delete '[JSON] (Response ()),
    _fileApiDownload ::
      route
        :- Description "download from server"
          :> "download"
          :> Capture "option" Option
          :> Capture "file_id" (Id "file")
          :> QueryParam "width" Int
          :> QueryParam "height" Int
          :> RawM
  }
  deriving stock (Generic)
