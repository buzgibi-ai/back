{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Generic response that should be used in services. Currently it doesn't
-- keep type level info about concrete error that may happen, but that may
-- change in the future. Instead we use "Scaffold.Error" module to represent
-- generic errors without looking "inside".
--
-- Module provide 'Response' type and convenient pattern synonyms for
-- different pattern creation.
module Buzgibi.Transport.Response
  ( Response (Response, Ok, Warnings, Errors, Error),
    Buzgibi.AsError (..),
    fromValidation,
    fromEither,
    fromEithers,
    liftMaybe,
    toEither
  )
where

import Buzgibi.Transport.Error as Buzgibi
import Control.Lens hiding ((.=))
import Data.Aeson.Extended hiding (Error)
import Data.Swagger hiding (Response)
import qualified Data.Text as T
import Data.Typeable
import GHC.Exts
import GHC.Generics
import Test.QuickCheck.Extended
import Validation

--  Generic response for sirius services.
data Response a = Response
  { -- | Computation result.
    responseResult :: Maybe a,
    responseWarnings :: [Error],
    responseErrors :: [Error]
  }
  deriving stock (Show)
  deriving stock (Typeable)
  deriving stock (Generic)
  deriving stock (Foldable)
  deriving stock (Traversable)
  deriving stock (Functor)

pattern Ok :: a -> Response a
pattern Ok x = Response (Just x) ([] :: [Buzgibi.Error]) ([] :: [Buzgibi.Error])

pattern Warnings :: a -> [Error] -> Response a
pattern Warnings x warns = Response (Just x) warns ([] :: [Buzgibi.Error])

pattern Errors :: [Error] -> Response a
pattern Errors errs = Response Nothing [] errs

pattern Error :: Error -> Response a
pattern Error err = Response Nothing [] [err]

instance ToJSON a => ToJSON (Response a) where
  toJSON (Response Nothing [] []) = object ["success" .= Null]
  toJSON (Response Nothing ys xs) =
    object $
      (if null ys then id else (("warnings" .= ys) :))
        (["errors" .= xs | not (null xs)])
  toJSON (Response (Just x) ys xs) =
    object
      $ (if null ys then id else (("warnings" .= ys) :))
        . (if null xs then id else (("errors" .= xs) :))
      $ ["success" .= x]

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "response" $ \o -> do
    msuccess <- o .:? "success"
    warns <- o .:? "warnings" .!= []
    errors <- o .:? "errors" .!= []
    pure $ Response msuccess warns errors

instance (ToSchema a, Typeable a) => ToSchema (Response a) where
  declareNamedSchema _ = do
    eSchema <- declareSchemaRef (Proxy @[Error])
    aSchema <- declareSchemaRef (Proxy @a)
    let ident = T.pack $ show (typeRep (Proxy @a))
    pure $
      NamedSchema (Just $ "Response." <> ident) $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ fromList
              [ ("success", aSchema),
                ("warnings", eSchema),
                ("errors", eSchema)
              ]

instance Arbitrary a => Arbitrary (Response a) where arbitrary = fmap (\x -> Response x [] []) arbitrary

fromValidation :: Validation [Buzgibi.Error] a -> Response a
fromValidation v = either Errors Ok $ validationToEither v

fromEither :: AsError e => Either e a -> Response a
fromEither = either (Errors . flip (:) [] . asError) Ok

fromEithers :: AsError e => Either [e] a -> Response a
fromEithers = either (Errors . map asError) Ok

liftMaybe :: AsError e => Maybe a -> e -> Response a
liftMaybe (Just x) _ = Ok x
liftMaybe Nothing e = Buzgibi.Transport.Response.Error (asError e)

toEither :: Response a -> Either [Error] a
toEither (Response (Just x) _ _) = Right x
toEither (Response Nothing _ es) = Left es