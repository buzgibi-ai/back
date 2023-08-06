{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.WithField (WithField (..), getFirst) where

import Control.DeepSeq
import GHC.Generics
import GHC.TypeLits
import Data.Bifunctor
import Data.Aeson
import Data.Swagger
import qualified Data.Aeson.KeyMap as K
import Data.Proxy (Proxy (..))
import Control.Lens hiding ((.=))
import GHC.Exts
import qualified Data.List as L
import Data.String.Conv (toS)
import Test.QuickCheck.Arbitrary

-- | Injects field 'a' into 'b' with tag 's'. It has
-- special instances for 'ToJSON' and 'FromJSON' for
-- such injection and corresponding Swagger 'ToSchema'
-- instance.
--
-- For instance:
--
-- >>> encode (WithField "val" (Left 42) :: WithField "injected" String (Either Int Int))
-- "{\"Left\":42,\"id\":\"val\"}"
--
-- If the instance cannot inject field (in case of single values and arrays),
-- it wraps the result in the following way:
--
-- >>> encode (WithField "val" 42 :: WithField "injected" String Int)
-- "{\"value\":42,\"injected\":\"val\"}"
--
-- `WithField s a b` always overwites field `s` in JSON produced by `b`.
data WithField (s :: Symbol) a b = WithField !a !b
  deriving (Generic, Eq, Show, Read)

instance (NFData a, NFData b) => NFData (WithField s a b)

instance Functor (WithField s a) where
  fmap f (WithField a b) = WithField a (f b)

instance Bifunctor (WithField s) where
  bimap fa fb (WithField a b) = WithField (fa a) (fb b)

instance (KnownSymbol s, ToJSON a, ToJSON b) => ToJSON (WithField (s :: Symbol) a b) where
  toJSON (WithField x y) = inject x (toJSON y)
    where 
      inject x (Object obj) =
        Object $ K.insert (fromString (symbolVal (Proxy @s))) (toJSON x) obj
      inject x _ = object [ "value" .= toJSON y, fromString (symbolVal (Proxy @s)) .= toJSON x ]

instance (KnownSymbol s, FromJSON a, FromJSON b) => FromJSON (WithField s a b) where
  parseJSON obj = 
    flip (withObject "WithField") obj $ \o -> do 
      x <- o .: fromString (symbolVal (Proxy @s))
      let o' = K.delete (fromString (symbolVal (Proxy @s))) o
      y <- parseJSON $ Object o'
      return $ WithField x y

instance (KnownSymbol s, ToSchema a, ToSchema b) => ToSchema (WithField s a b) where
  declareNamedSchema _ = do
     NamedSchema n s <- declareNamedSchema (Proxy :: Proxy b)
     if s ^. type_ == Just SwaggerObject then inline n s
     else wrapper n s
     where
       field = toS $ symbolVal (Proxy :: Proxy s)
       namePrefix = "WithField '" <> field <> "' "
       wrapper n s = do
         indexSchema <- declareSchema (Proxy :: Proxy a)
         return $ NamedSchema (fmap (namePrefix <>) n) $ mempty
            & type_ .~ Just SwaggerObject
            & properties .~ fromList
                [ ("value", Inline s)
                , (field, Inline indexSchema)
                ]
            & required .~ (L.nub [ "value", field ])
       inline n s = do
        indexSchema <- declareSchema (Proxy :: Proxy a)
        return $ NamedSchema (fmap (namePrefix <>) n) $ s
            & properties %~ (fromList [(field, Inline indexSchema)] <>)
            & required %~ ([field] <>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (WithField s a b) where 
  arbitrary = WithField <$> arbitrary <*> arbitrary

getFirst :: WithField (s :: Symbol) a b -> a
getFirst (WithField x _) = x