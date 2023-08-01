{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Buzgibi.Api.Controller.Frontend.Translate (controller) where

import Buzgibi.Transport.Model.Translation
import Buzgibi.Api.Controller.Utils (ContentError (..), getContent, withError)
import Buzgibi.EnvKeys (key, translation)
import Buzgibi.Transport.Response
import Control.Lens
import Control.Lens.Iso.Extended (stext, textbs)
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Traversable (for)
import GHC.Exts
import qualified GitHub as GitHub
import Katip.Controller

data Error = Github | Content ContentError

instance Show Error where
  show Github = "cannot find resource on github"
  show (Content e) = show e

controller :: Lang -> KatipControllerM (Response Translation)
controller lang = do
  github <- fmap (^. katipEnv . github) ask
  let docs_repo = do
        val <- github
        let repo = val ^. translation
        pure (val ^. key, repo)

  file <- fmap (join . maybeToRight (Content Resource404)) $
    for docs_repo $ \(key, repo) ->
      liftIO $
        fmap (first (const Github)) $
          GitHub.github (GitHub.OAuth (key ^. textbs)) $
            GitHub.contentsForR
              "buzgibi-ai"
              (fromString (repo ^. from stext))
              (show lang ^. stext <> ".yaml")
              Nothing
  return $ withError (first Content (getContent @Translation file)) id
