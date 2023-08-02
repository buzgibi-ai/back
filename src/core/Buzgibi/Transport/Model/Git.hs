{-# LANGUAGE OverloadedStrings #-}

module Buzgibi.Transport.Model.Git (Webhook (..)) where

import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V

data Webhook = Webhook { files :: ![T.Text] }
  deriving Show

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o -> do
    commits <- o .: "commits"
    let getFiles = 
          withObject 
          "Webhook(array):item" 
          (flip (.:) "modified")
    fmap (Webhook . concat . V.toList) $ 
      withArray "SAResponse(array)" 
        (traverse getFiles) commits