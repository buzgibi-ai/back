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
{-# LANGUAGE TupleSections #-}

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
import qualified Control.Monad.State.Class as S
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Control.Concurrent.Async as Async
import Data.Either (fromRight)
import BuildInfo (location)
import Katip
import Data.Functor (($>))

data Error = Github | Content ContentError

instance Show Error where
  show Github = "cannot find resource on github"
  show (Content e) = show e

controller :: Lang -> KatipControllerM (Response Translation)
controller lang = do
  State translations <- S.get
  
  if Map.null translations
  then  do
    github <- fmap (^. katipEnv . github) ask
    let docs_repo = do
          val <- github
          let repo = val^.translation
          pure (val ^. key, repo)

    files <- fmap (join . maybeToRight (Content Resource404)) $
      for docs_repo $ \(key, repo) ->
        liftIO $ fmap sequence $ 
          Async.forConcurrently [English .. Turkish] $ \lang -> 
            fmap (bimap (const Github) (lang,) . getContent @Translation) $
              GitHub.github (GitHub.OAuth (key ^. textbs)) $
                GitHub.contentsForR
                  "buzgibi-ai"
                  (fromString (repo ^. from stext))
                  (show lang ^. stext <> ".yaml")
                  Nothing
    $(logTM) InfoS $ logStr $ $location <> " translation first load ---> " <> show (files^.._Right.traverse._1)
    S.modify $ \_ -> State $ stateToMap $ fromRight mempty files
    return $ withError (join (fmap (maybeToRight (Content Resource404) . lookup lang) files)) id
  else $(logTM) InfoS (logStr @String ($location <> " translation from cache ")) $>
        (fromEither $ first (T.pack . show) $ maybeToRight (Content Resource404) $ Map.lookup lang translations)