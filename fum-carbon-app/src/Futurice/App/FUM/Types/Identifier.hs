{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Futurice.App.FUM.Types.Identifier (
    Identifier (..),
    _IdentifierText,
    identifierToText,
    Entity(..),
    ) where

import Data.Swagger      (SwaggerType (SwaggerString), format, type_)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Swagger.Internal.Schema       as Swagger
import qualified Data.UUID                          as UUID
import qualified Database.PostgreSQL.Simple.ToField as Postgres

newtype Identifier a = Identifier UUID
    deriving (Eq, Ord, Show, Typeable, Generic)

identifierToText :: Identifier a -> Text
identifierToText (Identifier u) = UUID.toText u

_IdentifierText :: Prism' Text (Identifier a)
_IdentifierText = prism' identifierToText (fmap Identifier . UUID.fromText)

instance NFData (Identifier a)

instance Arbitrary (Identifier a) where
    arbitrary = Identifier <$> arbitrary

instance HasUUID (Identifier a) where
    uuid = lens (\(Identifier u) -> u) (\_ u -> Identifier u)

instance ToHttpApiData (Identifier a) where
    toUrlPiece   = toUrlPiece . view uuid
    toQueryParam = toQueryParam . view uuid

instance FromHttpApiData (Identifier a) where
    parseUrlPiece   = fmap Identifier . parseUrlPiece
    parseQueryParam = fmap Identifier . parseQueryParam

instance Entity a => ToParamSchema (Identifier a) where
    toParamSchema _ = mempty
        & type_ .~ Just SwaggerString
        & format ?~ "Identifier " <> entityName (Proxy :: Proxy a) <> ": uuid"

instance ToJSON (Identifier a) where
    toJSON (Identifier u) = toJSON u

instance FromJSON (Identifier a) where
    parseJSON = fmap Identifier . parseJSON

instance Postgres.ToField (Identifier a) where
    toField (Identifier i) = Postgres.toField i

-------------------------------------------------------------------------------
-- Entity
-------------------------------------------------------------------------------

-- | Class of entities.
class Entity a where
    entityName :: Proxy a -> Text

-------------------------------------------------------------------------------
-- ToSchema
-------------------------------------------------------------------------------

instance Entity a => ToSchema (Identifier a) where
    declareNamedSchema _ = do
        s <- declareNamedSchema (Proxy :: Proxy UUID)
        return (Swagger.rename (Just $ "Identifier " <> entityName p) s)
      where
        p = Proxy :: Proxy a
