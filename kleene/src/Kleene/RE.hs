{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kleene.RE where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice    (BoundedJoinSemiLattice (..), JoinSemiLattice (..))
import Data.List          (foldl')
import Data.RangeSet.List (RSet)
import Data.Semigroup     (Semigroup (..))
import Data.Set           (Set)
import Data.String        (IsString (..))

import qualified Data.RangeSet.List as RSet
import qualified Data.Set           as Set

import Kleene.Sets

-- | Regular expression
--
-- Constructors are exposed, but you should use
-- smart constructors in this module to construct 'RE'.
--
-- The 'Eq' and 'Ord' instances are structural. See 'eq', 'ne' if you need
-- regular-expression equivalence.
--
-- TODO: note greediness
data RE c
    = REChars (RSet c)      -- ^ Single character
    | REAppend [RE c]       -- ^ Concatenation
    | REUnion (Set (RE c))  -- ^ Union
    | REStar (RE c)         -- ^ Kleene star
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Smart constructor
-------------------------------------------------------------------------------

empty :: RE c
empty = REChars RSet.empty

eps :: RE c
eps = REAppend []

char :: c -> RE c
char = REChars . RSet.singleton

appends :: Eq c => [RE c] -> RE c
appends rs0
    | elem empty rs1 = empty
    | otherwise = case rs1 of
        [r] -> r
        rs  -> REAppend rs
  where
    rs1 = flatten rs0

    flatten rs = concatMap f rs0
    f (REAppend rs) = rs
    f r             = [r]

star :: Eq c => RE c -> RE c
star r@(REStar _) = r
star r
    | r == eps    = r
    | r == empty  = eps
    | otherwise     = REStar r

string :: [c] -> RE c
string = REAppend . map (REChars . RSet.singleton)

-------------------------------------------------------------------------------
-- derivative
-------------------------------------------------------------------------------

nullable :: RE a -> Bool
nullable (REChars _)   = False
nullable (REAppend rs) = all nullable rs
nullable (REUnion rs)  = any nullable rs
nullable (REStar _)    = True

derivateAppend :: Ord c => c -> [RE c] -> RE c
derivateAppend _ []      = empty
derivateAppend c [r]     = derivate c r
derivateAppend c (r:rs)
    | nullable r       = r' <> appends rs \/ rs'
    | otherwise          = r' <> appends rs
  where
    r'  = derivate c r
    rs' = derivateAppend c rs

derivate :: Ord c => c -> RE c -> RE c
derivate c (REChars cs)
  | c `RSet.member` cs      = eps
  | otherwise               = empty
derivate c (REUnion rs)     = REUnion (Set.map (derivate c) rs)
derivate c (REAppend rs)    = derivateAppend c rs
derivate c rs@(REStar r)    = derivate c r <> rs

matches :: Ord c => RE c -> [c] -> Bool
matches r = nullable . foldl' (flip derivate) r

-------------------------------------------------------------------------------
-- States
-------------------------------------------------------------------------------

-- |
--
-- >>> traverse_ print $ states (star (char A))
-- REChars fromRangeList []
-- REStar (REChars fromRangeList [(A,A)])
--
-- >>> traverse_ print $ states $ string [A, B]
-- REChars fromRangeList []
-- REChars fromRangeList [(B,B)]
-- REAppend []
-- REAppend [REChars fromRangeList [(A,A)],REChars fromRangeList [(B,B)]]
states :: forall a. (Enum a, Bounded a, Ord a) => RE a -> Set (RE a)
states re = go mempty [re]  where
    go :: Set (RE a) -> [RE a] -> Set (RE a)
    go !acc [] = acc
    go acc (r : rs)
        | r `Set.member` acc =
            go acc rs
        | otherwise =
            go (Set.insert r acc) (map (flip derivate r) cs ++ rs)
      where
        -- TODO: check leading chars of 'r'
        cs = [minBound .. maxBound]

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq c => Semigroup (RE c) where
    r <> r' = appends [r, r']

instance Eq c => Monoid (RE c) where
    mempty  = eps
    mappend = (<>)

instance Ord c => JoinSemiLattice (RE c) where
    r \/ r'
        | r  == empty = r'
        | r' == empty = r
    REUnion rs \/ REUnion rs' = REUnion (rs <> rs')
    REUnion rs \/ r           = REUnion (Set.insert r rs)
    r          \/ REUnion rs  = REUnion (Set.insert r rs)
    r          \/ r'          = REUnion (Set.fromList [r, r'])

instance Ord c => BoundedJoinSemiLattice (RE c) where
    bottom = empty

instance c ~ Char => IsString (RE c) where
    fromString = string

-------------------------------------------------------------------------------
-- JavaScript
-------------------------------------------------------------------------------

toJS :: RE Char -> String
toJS inputRe = showString "^" . go False inputRe . showString "$" $ "" where
    go :: Bool -> RE Char -> ShowS
    go p (REStar a)
        = parens p
        $ go True a . showChar '*'
    go p (REAppend rs)
        = parens p $ goMany id rs
    go p (REUnion rs) = case rs' of
        [r, s]
            | r == eps -> parens p $ go True s . showChar '?'
            | s == eps -> parens p $ go True r . showChar '?'
        _ -> parens True $ goMany (showChar '|') rs'
      where
        rs' = Set.toList rs
    go _ (REChars cs)
        | cs ==  RSet.full             = showString "[^]"
        | cs == dotRSet                = showString "."
        | RSet.size cs == 1            = showChar (head (RSet.elems cs))
        | RSet.size cs < RSet.size ics = rsetToJS cs
        | otherwise                    = rsetToJS ics
      where
        ics = RSet.complement cs

    goMany :: ShowS -> [RE Char] -> ShowS
    goMany sep = foldr (\a b -> go False a . sep . b) id

    parens :: Bool -> ShowS -> ShowS
    parens True  s = showString "(?:" . s . showChar ')'
    parens False s = s

-- TODO: escaping, ranges
rsetToJS :: RSet Char -> ShowS
rsetToJS cs
    = showChar '['
    . foldMap f (RSet.toRangeList cs)
    . showChar ']'
  where
    f (a, b)
      | a == b = showChar a
      | otherwise = showChar a . showChar '-' . showChar b

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
-- >>> data AB = A | B deriving (Eq, Ord, Show, Enum, Bounded)
-- >>> import Data.Foldable (traverse_)
