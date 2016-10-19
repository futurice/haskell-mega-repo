{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Identifier (
    Identifier(..),
    HasIdentifier(..),
    ) where

import PlanMill.Internal.Prelude

import qualified Data.Csv     as Csv
import           Data.Swagger (ToSchema)

import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.FromField as Postgres

-- | Tagged identifier
newtype Identifier a = Ident Word64
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''Identifier

instance NFData (Identifier a)
instance Hashable (Identifier a)
instance AnsiPretty (Identifier a)
instance Binary (Identifier a)
instance HasStructuralInfo (Identifier a) where structuralInfo = sopStructuralInfo
instance HasSemanticVersion (Identifier a)

instance FromJSON (Identifier a) where
    parseJSON = fmap Ident . parseJSON
instance ToJSON (Identifier a) where
    toJSON (Ident i) = toJSON i

-- | Identities with identifier.
class HasIdentifier entity super | entity -> super where
    identifier :: Lens' entity (Identifier super)

instance HasIdentifier (Identifier a) a where
    identifier = lens id (flip const)

instance Csv.ToField (Identifier a) where
    toField (Ident x) = Csv.toField x

instance ToSchema (Identifier a)

instance Postgres.ToField (Identifier a) where
    toField (Ident x) = Postgres.toField x

instance Postgres.FromField (Identifier a) where
    fromField f mbs = Ident . fromInteger <$> Postgres.fromField f mbs
