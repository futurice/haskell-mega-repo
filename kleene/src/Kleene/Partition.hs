module Kleene.Partition where

import Control.Applicative (liftA2)
import Data.Foldable       (fold, toList)
import Data.List           (foldl')
import Data.RangeSet.List  (RSet)
import Data.Semigroup      (Semigroup (..))
import Data.Set            (Set)

import qualified Data.RangeSet.List as RSet
import qualified Data.Set           as Set

import Test.QuickCheck

-- | ...
newtype Partition a  = Partition { unPartition :: Set (RSet a) }
  deriving (Eq, Ord)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Show a => Show (Partition a) where
    showsPrec d (Partition rs)
        = showParen (d > 10)
        $ showString "fromRSets "
        . showsPrec 11 (Set.toList rs)

instance (Enum a, Bounded a, Ord a, Arbitrary a) => Arbitrary (Partition a) where
    arbitrary = oneof [ atomic, liftA2 (<>) atomic atomic ]
      where
        atomic = oneof
            [ singletonP . RSet.singletonRange <$> arbitrary
            , split <$> arbitrary
            , fromRSets . map RSet.fromRangeList <$> arbitrary
            ]

-- | See 'wedge'.
instance (Enum a, Bounded a, Ord a) => Semigroup (Partition a) where
    (<>) = wedge

instance (Enum a, Bounded a, Ord a) => Monoid (Partition a) where
    mempty = fullP
    mappend = (<>)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

-- | Construct 'Partition' from list of 'RSet's.
--
-- prop> invariant $ asPartitionChar $ fromRSets xs
--
fromRSets :: (Enum a, Bounded a, Ord a) => [RSet a] -> Partition a
fromRSets = foldl' (\p r -> p <> singletonP r) fullP

singletonP :: (Enum a, Bounded a, Ord a) => RSet a -> Partition a
singletonP r
    | r == RSet.empty = fullP
    | r == RSet.full  = fullP
    | otherwise       = Partition $ Set.fromList [ RSet.full RSet.\\ r, r]

fullP :: (Enum a, Bounded a, Ord a) => Partition a
fullP = Partition $ Set.singleton RSet.full

-------------------------------------------------------------------------------
-- Querying
-------------------------------------------------------------------------------

-- | Count of sets in a 'Partition'.
--
-- >>> size fullP
-- 1
--
-- >>> size $ split (10 :: Word8)
-- 2
--
-- prop> size (asPartitionChar p) >= 1
--
size :: Partition a -> Int
size (Partition xs) = length xs

-- | Extract examples from each subset in a 'Partition'.
--
-- >>> examples $ split (10 :: Word8)
-- fromList [0,11]
--
-- >>> examples $ split (10 :: Word8) <> split 20
-- fromList [0,11,21]
--
-- prop> size (asPartitionChar p) == length (examples p)
--
examples :: Ord a => Partition a -> Set a
examples (Partition xs) = Set.map RSet.findMin xs

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Wedge partitions.
--
-- TODO: wherefrom we get a name?
--
-- >>> split (10 :: Word8) <> split 20
-- fromRSets [fromRangeList [(0,10)],fromRangeList [(11,20)],fromRangeList [(21,255)]]
--
-- prop> fullP `wedge` (p :: Partition Char) == p
-- prop> (p :: Partition Char) <> fullP == p
-- prop> asPartitionChar p <> q == q <> p -- commutative
-- prop> asPartitionChar p <> p == p      -- idempotent
-- prop> invariant $ asPartitionChar p <> q
-- prop> wedgeInvariant (asPartitionChar p) q (p <> q)
--
wedge :: Ord a => Partition a -> Partition a -> Partition a
wedge (Partition as) (Partition bs)
    = Partition
    $ Set.delete RSet.empty -- is it necessary?
    $ Set.fromList
    [ a `RSet.intersection` b | a <- Set.toList as, b <- Set.toList bs ]

-- | Check that @let r = p <> q in 'wedgeInvariant' p q r
--
-- * all sets in @r@ are subset of some set in @p@
--
-- * all sets in @r@ are subset of some set in @q@
--
-- Also @r@ is the smallest such 'Partition', i.e. for all 'Partition' which
-- satisfy the properties above, @s@, @'size' s >= 'size' r@
--
wedgeInvariant :: (Enum a, Bounded a, Ord a)
    => Partition a -> Partition a -> Partition a -> Bool
wedgeInvariant (Partition xs) (Partition ys) (Partition zs) =
    iSubsetLeft && iSubsetRight
  where
    iSubsetLeft  = all (\x -> any (RSet.isSubsetOf x) xs) zs
    iSubsetRight = all (\x -> any (RSet.isSubsetOf x) ys) zs

-- | Simplest partition: given @x@ partition space into @[min..x] and (x .. max]@
--
-- >>> split (127 :: Word8)
-- fromRSets [fromRangeList [(0,127)],fromRangeList [(128,255)]]
--
-- prop> invariant $ split (c :: Char)
--
split :: (Enum a, Bounded a, Ord a) => a -> Partition a
split x
    | x == maxBound = fullP
    | otherwise = Partition $ Set.fromList
        [ RSet.singletonRange (minBound, x)
        , RSet.singletonRange (succ x, maxBound)
        ]

-- | Check 'partition' invariants.
--
-- Given @a@ is a non-empty type, 'Partiion' is:
--
-- * not null
--
-- * complete i.e. spans whole range of @a@. implies first invariant.
--
-- * disjoint, i.e. 'RSet's are non-intersecting.
--
-- * doesn't contain empty 'RSet's.
--
-- prop> size (asPartitionChar p) >= 1
-- prop> RSet.full == fold (unPartition (p :: Partition Char))
-- prop> and [ RSet.null (RSet.intersection a b) | (a, b) <- pairs $ unPartition (p :: Partition Char) ]
-- prop> all (not . RSet.null) (unPartition (p :: Partition Char))
--
invariant :: (Enum a, Bounded a, Ord a) => Partition a -> Bool
invariant (Partition rs) =
    iNotNull && iComplete && iDisjoint && iNoNulls
  where
    iNotNull  = not (null rs)
    iComplete = RSet.full == fold rs
    iDisjoint = and [ RSet.null (RSet.intersection a b) | (a, b) <- pairs rs ]
    iNoNulls  = all (not . RSet.null) rs

-- | Make all pairs of the list.
--
-- >>> pairs [1..3]
-- [(1,2),(1,3),(2,3)]
--
-- >>> pairs [1..5]
-- [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]
--
pairs :: Foldable f => f a -> [(a, a)]
pairs = pairs' . toList where
    pairs' [] = []
    pairs' [_] = []
    pairs' (x : xs) = map ((,) x) xs ++ pairs xs

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Word
-- >>> let asPartitionChar :: Partition Char -> Partition Char; asPartitionChar = id
-- >>> instance (Ord a, Enum a, Arbitrary a) => Arbitrary (RSet a) where arbitrary = fmap RSet.fromRangeList arbitrary
