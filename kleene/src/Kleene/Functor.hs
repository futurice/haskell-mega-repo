{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Kleene.Functor where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice     ((\/))
import Control.Applicative (Alternative (..), liftA2)
import Data.RangeSet.List  (RSet)
import Data.Semigroup      (Semigroup (..))
import Data.String         (IsString (..))

import qualified Data.RangeSet.List     as RSet
import qualified Text.Regex.Applicative as R

import qualified Kleene.RE as RE

import Kleene.Sets

data Greediness = Greedy | NonGreedy
  deriving (Eq, Ord, Show, Enum, Bounded)

data Kleene c a where
    KleeneEmpty  :: Kleene c a
    KleenePure   :: a -> Kleene c a
    KleeneChar   :: (Ord c, Enum c) => RSet c -> Kleene c c
    KleeneAppend :: (a -> b -> r) -> Kleene c a -> Kleene c b -> Kleene c r
    KleeneUnion  :: Kleene c a -> Kleene c a -> Kleene c a
    KleeneStar   :: Greediness -> Kleene c a -> Kleene c [a]

    -- optimisations
    KleeneMap    :: (a -> b) -> Kleene c a -> Kleene c b -- could use Pure and  Append
    KleeneString :: Eq c => [c] -> Kleene c [c]          -- could use Char and Append

instance (c ~ Char, IsString a) => IsString (Kleene c a) where
    fromString s = KleeneMap fromString (KleeneString s)

instance Functor (Kleene c) where
    fmap _ KleeneEmpty          = KleeneEmpty
    fmap f (KleenePure x)       = KleenePure (f x)
    fmap f (KleeneMap g k)      = KleeneMap (f . g) k
    fmap f (KleeneAppend g a b) = KleeneAppend (\x y -> f (g x y)) a b
    fmap f k                    = KleeneMap f k

instance Applicative (Kleene c) where
    pure = KleenePure

    KleeneEmpty <*> _ = KleeneEmpty
    _ <*> KleeneEmpty = KleeneEmpty

    KleenePure f <*> k = fmap f k
    k <*> KleenePure x = fmap ($x) k

    f <*> x = KleeneAppend ($) f x
    -- TODO: re-associate trees?

instance Alternative (Kleene c) where
    empty = KleeneEmpty

    KleeneEmpty <|> k = k
    k <|> KleeneEmpty = k
    KleeneChar a <|> KleeneChar b = KleeneChar (RSet.union a b)

    a <|> b = KleeneUnion a b

    many KleeneEmpty      = KleenePure []
    many (KleeneStar _ k) = KleeneMap pure (KleeneStar Greedy k)
    many k                = KleeneStar Greedy k

    some KleeneEmpty      = KleeneEmpty
    some (KleeneStar _ k) = KleeneMap pure (KleeneStar Greedy k)
    some k                = liftA2 (:) k (KleeneStar Greedy k)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | >>> putStrLn $ kleeneToJS kleeneAnyChar
-- ^[^]$
kleeneAnyChar :: (Ord c, Enum c, Bounded c) => Kleene c c
kleeneAnyChar = KleeneChar RSet.full

-- | >>> putStrLn $ kleeneToJS kleeneDotChar
-- ^.$
kleeneDotChar :: Kleene Char Char
kleeneDotChar = KleeneChar dotRSet

-- | >>> putStrLn $ kleeneToJS kleeneEverything
-- ^[^]*$
kleeneEverything :: (Ord c, Enum c, Bounded c) => Kleene c [c]
kleeneEverything = many kleeneAnyChar

-- | >>> putStrLn $ kleeneToJS kleeneEverything1
-- ^[^][^]*$
kleeneEverything1 :: (Ord c, Enum c, Bounded c) => Kleene c [c]
kleeneEverything1 = some kleeneAnyChar

-- | Matches whole input?
--
-- TODO: this is quick test, not proper decision procedure
isKleeneEverything :: Bounded c => Kleene c a -> Bool
isKleeneEverything k = case k of
    KleeneMap _ (KleeneStar _ (KleeneChar cs)) -> cs == RSet.full
    KleeneStar _ (KleeneChar cs)               -> cs == RSet.full
    _                                          -> False

kleeneCharRange :: (Enum c, Ord c) => c -> c -> Kleene c c
kleeneCharRange a b = KleeneChar (RSet.singletonRange (a, b))

-------------------------------------------------------------------------------
-- RE
-------------------------------------------------------------------------------

kleeneToRe :: Ord c => Kleene c a -> RE.RE c
kleeneToRe (KleeneMap _ a)      = kleeneToRe a
kleeneToRe (KleeneUnion a b)    = kleeneToRe a \/ kleeneToRe b
kleeneToRe (KleeneAppend _ a b) = kleeneToRe a <> kleeneToRe b
kleeneToRe (KleeneStar _ a)     = RE.star (kleeneToRe a)
kleeneToRe (KleeneString s)     = RE.string s
kleeneToRe KleeneEmpty          = RE.empty
kleeneToRe (KleenePure _)       = RE.eps
kleeneToRe (KleeneChar cs)      = RE.REChars cs

-------------------------------------------------------------------------------
-- regex-applicative
-------------------------------------------------------------------------------

-- | Convert 'Kleene' to 'RE' from @regex-applicative@.
--
-- TODO: match example
kleeneToRA :: Kleene c a -> R.RE c a
kleeneToRA KleeneEmpty              = empty
kleeneToRA (KleenePure x)           = pure x
kleeneToRA (KleeneChar cs)          = R.psym (\c -> RSet.member c cs)
kleeneToRA (KleeneAppend f a b)     = liftA2 f (kleeneToRA a) (kleeneToRA b)
kleeneToRA (KleeneUnion a b)        = kleeneToRA a <|> kleeneToRA b
kleeneToRA (KleeneStar Greedy a)    = many (kleeneToRA a)
kleeneToRA (KleeneStar NonGreedy a) = R.few (kleeneToRA a)
kleeneToRA (KleeneMap f a)          = fmap f (kleeneToRA a)
kleeneToRA (KleeneString s)         = R.string s

-------------------------------------------------------------------------------
-- JavaScript
-------------------------------------------------------------------------------

-- | Convert to non-matching JavaScript string which can be used
-- as an argument to 'new RegExp'
--
-- TODO: use builder
-- TODO: escaping
--
-- >>> putStrLn $ kleeneToJS "foobar"
-- ^foobar$
--
-- >>> putStrLn $ kleeneToJS $ many "foobar"
-- ^(?:foobar)*$
--
kleeneToJS :: Kleene Char a -> String
kleeneToJS = RE.toJS . kleeneToRe

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
--
-- >>> :set -XOverloadedStrings
