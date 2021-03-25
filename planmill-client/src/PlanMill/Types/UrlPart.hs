{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
module PlanMill.Types.UrlPart (
    UrlParts(),
    (//),
    ToUrlPart(..),
    ToUrlParts(..),
    fromUrlParts,
    ) where

import PlanMill.Internal.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

import PlanMill.Types.Identifier

newtype UrlParts = UrlParts (NE.NonEmpty String)
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable UrlParts
instance NFData UrlParts
instance Binary UrlParts
instance Structured UrlParts

instance Semigroup UrlParts where
    UrlParts a <> UrlParts b = UrlParts (a <> b)

fromUrlParts :: UrlParts -> String
fromUrlParts (UrlParts ps) = fold . fmap ("/" ++) $ ps

part :: String -> UrlParts
part = UrlParts . (NE.:| [])

infixl 4 //
(//) :: (ToUrlParts a, ToUrlPart b) => a -> b -> UrlParts
a // b = toUrlParts a <> part (toUrlPart b)

class ToUrlPart a where
    toUrlPart :: a -> String

class ToUrlParts a where
    toUrlParts :: a -> UrlParts

instance ToUrlPart T.Text where
    toUrlPart = T.unpack

instance ToUrlPart (Identifier a) where
    toUrlPart (Ident i) = show i

instance ToUrlParts T.Text where
    toUrlParts = part . T.unpack

instance ToUrlParts (Identifier a) where
    toUrlParts = part . toUrlPart

instance ToUrlParts UrlParts where
    toUrlParts = id

instance ToJSON UrlParts
instance FromJSON UrlParts

