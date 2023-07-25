{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.WithField (WithField (..)) where

import Control.DeepSeq
import GHC.Generics
import GHC.TypeLits
import Data.Bifunctor
import Data.Aeson
import Data.Swagger
import qualified Data.Aeson.KeyMap as K
import Data.Proxy (Proxy (..))
import Control.Lens
import GHC.Exts


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
  toJSON (WithField x y) = inject (toJSON x) (toJSON y)
    where 
      inject (String s) (Object obj) = 
        Object $ K.insert (fromString (symbolVal (Proxy @s))) (toJSON s) obj
      inject _ _ = error "unsupported types"

instance (KnownSymbol s, FromJSON a, FromJSON b) => FromJSON (WithField s a b) where
  parseJSON obj = 
    flip (withObject "WithField") obj $ \o -> do 
      x <- o .: fromString (symbolVal (Proxy @s))
      y <- parseJSON obj
      return $ WithField x y

instance (KnownSymbol s, ToSchema a, ToSchema b) => ToSchema (WithField s a b) where
  declareNamedSchema _ = do
    NamedSchema nb sb <- declareNamedSchema (Proxy @a)
    NamedSchema na sa <- declareNamedSchema (Proxy @b)
    let combinedName a b = "WithFields_" <> a <> "_" <> b
    let newName = combinedName <$> na <*> nb
    let wrapper a = mempty
          & type_ ?~ SwaggerObject
          & properties .~ fromList  [ ((fromString (symbolVal (Proxy @s))), Inline a) ]
          & required .~ [ (fromString (symbolVal (Proxy @s))) ]
    return . NamedSchema newName $ 
      case (sa ^. type_ , sb ^. type_) of
        (_, Just SwaggerObject) -> sb <> wrapper sa
        _ -> error "unsupported types"