{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances               #-}
module Futurice.Report.Columns where

import Prelude ()
import Futurice.Prelude
import Futurice.List

import Generics.SOP (I (..), NP (..))

class ToColumns a where
    type Columns a :: [*]
    
    toColumns :: a -> [NP I (Columns a)]

-------------------------------------------------------------------------------
-- Scalars
-------------------------------------------------------------------------------

instance ToColumns () where
    type Columns () = '[]

    toColumns _ = []

-------------------------------------------------------------------------------
-- Containers
-------------------------------------------------------------------------------

instance ToColumns a => ToColumns [a] where
    type Columns [a] = Columns a

    toColumns = concatMap toColumns

instance (ToColumns a, ToColumns b) => ToColumns (a, b) where
    type Columns (a, b) = Append (Columns a) (Columns b)

    toColumns (a, b) = append <$> toColumns a <*> toColumns b

instance ToColumns a => ToColumns (Vector a) where
    type Columns (Vector a) = Columns a

    toColumns = foldMap toColumns

instance (ToColumns k, ToColumns v) => ToColumns (Map k v) where
    type Columns (Map k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

-- TODO: https://github.com/ekmett/lens/pull/679
instance (Eq k, Hashable k, ToColumns k, ToColumns v) => ToColumns (HashMap k v) where
    type Columns (HashMap k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v
