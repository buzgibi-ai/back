{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.Swagger.Schema.Extended
  ( schemaOptions,
    schemaOptionsDef,
    deriveToSchema,
    deriveToSchemaDef,
    deriveToSchemaFieldLabelModifier,
    deriveToSchemaConstructorTag,
    deriveToSchemaConstructorTagUnrestricted,
    module Data.Swagger.Schema,
    modify,
  )
where

import Control.Lens ((^.))
import Control.Lens.Iso.Extended (stext)
import Data.Aeson (defaultOptions)
import Data.Aeson.Extended (aesonOptions)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger
import Data.Swagger.Schema
import Data.Time.Clock (DiffTime)
import Data.Typeable (Typeable)
import qualified Data.Typeable as T (typeRep)
import Language.Haskell.TH
import Type.Reflection (typeRep)

schemaOptionsDef :: SchemaOptions
schemaOptionsDef = fromAesonOptions defaultOptions

schemaOptions :: String -> SchemaOptions
schemaOptions = fromAesonOptions . aesonOptions

deriveToSchema :: Name -> Q [Dec]
deriveToSchema name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema =
        genericDeclareNamedSchema (schemaOptions $sname)
    |]
  where
    sname = return (LitE (StringL (nameBase name)))

deriveToSchemaDef :: Name -> Q [Dec]
deriveToSchemaDef name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema =
        genericDeclareNamedSchema schemaOptionsDef
    |]

deriveToSchemaFieldLabelModifier :: Name -> Q Exp -> Q [Dec]
deriveToSchemaFieldLabelModifier name modify =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema =
        genericDeclareNamedSchema @($(conT name)) $ defaultSchemaOptions {fieldLabelModifier = $modify}
    |]

deriveToSchemaConstructorTag :: Name -> Q Exp -> Q [Dec]
deriveToSchemaConstructorTag name modify =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema =
        genericDeclareNamedSchema @($(conT name)) $ defaultSchemaOptions {constructorTagModifier = $modify}
    |]

deriveToSchemaConstructorTagUnrestricted :: Name -> Q Exp -> Q [Dec]
deriveToSchemaConstructorTagUnrestricted name modify =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema =
        genericDeclareNamedSchemaUnrestricted @($(conT name)) $ defaultSchemaOptions {constructorTagModifier = $modify}
    |]

instance ToSchema DiffTime where
  declareNamedSchema _ = pure $ NamedSchema (Just $ (show (typeRep @DiffTime)) ^. stext) $ toSchema (Proxy @Double)

modify :: forall a. Typeable a => Proxy a -> String -> String
modify proxy =
  \s ->
    let (head : tail) =
          show (T.typeRep proxy)
     in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
