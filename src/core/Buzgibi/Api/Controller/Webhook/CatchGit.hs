{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Buzgibi.Api.Controller.Webhook.CatchGit (controller) where


import Buzgibi.Transport.Payload (Payload (..))
import Buzgibi.Transport.Model.Git (Webhook (..))
import qualified Buzgibi.Api.Controller.Frontend.Translate as Frontend.Translate (controller)
import BuildInfo (location)
import Katip.Controller
import Katip
import Data.Aeson (encode, eitherDecode')
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)
import Data.Monoid (Any (..))
import Data.Default.Class
import Control.Monad (when)
import qualified Control.Monad.State.Class as S
import qualified Data.Map.Strict as Map
import Control.Exception.Lifted (onException)

controller :: Payload -> KatipControllerM ()
controller (Payload payload) = do
  $(logTM) DebugS $ logStr $ $location <> " catch git webhook, payload ---> " <> show payload
  let webhooke = eitherDecode' @Webhook $ encode payload
  res <- for webhooke $ \Webhook {..} -> do
    let isChanged = 
          getAny $ flip foldMap files $ \f -> 
           Any $ f == "english.yaml" || f == "turkish.yaml"
    when isChanged $ do
      old <- S.state (, State Map.empty)
      fmap (const ()) (Frontend.Translate.controller def) `onException` S.put old
  whenLeft res $ \e -> $(logTM) ErrorS $ logStr $ $location <> " git webhook, error ---> " <> show e