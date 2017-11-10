{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Vec where

import Futurice.Peano
import Futurice.Prelude
import Prelude ()

infixr 5 :::

data Vec (n :: Peano) a where
    VNil  :: Vec 'PZ a
    (:::) :: a -> Vec n a -> Vec ('PS n) a

instance Show a => Show (Vec n a) where
    showsPrec _ VNil       = showString "VNil"
    showsPrec d (x ::: xs) = showParen (d > 5)
        $ showsPrec 6 x
        . showString " ::: "
        . showsPrec 5 xs 

instance Functor (Vec n) where
    fmap _ VNil       = VNil
    fmap f (x ::: xs) = f x ::: fmap f xs

instance Foldable (Vec n) where
    foldMap _ VNil = mempty
    foldMap f (x ::: xs) = mappend (f x) (foldMap f xs)

-- TODO: write more instances

-------------------------------------------------------------------------------
-- VecEach
-------------------------------------------------------------------------------

-- | Write functions on 'Vec'. Use them with tuples.
class VecEach s t a b | s -> a, t -> b, s b -> t, t a -> s where
    mapWithVec :: (forall n. Vec n a -> Vec n b) -> s -> t

instance (a ~ a', b ~ b') => VecEach (a, a') (b, b') a b where
    mapWithVec f (x, y) = case f (x ::: y ::: VNil) of
        x' ::: y' ::: VNil -> (x', y')

instance (a ~ a2, a ~ a3, b ~ b2, b ~ b3) => VecEach (a, a2, a3) (b, b2, b3) a b where
    mapWithVec f (x, y, z) = case f (x ::: y ::: z ::: VNil) of
        x' ::: y' ::: z' ::: VNil -> (x', y', z')
