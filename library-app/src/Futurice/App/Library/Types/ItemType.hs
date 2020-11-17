{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Futurice.App.Library.Types.ItemType where

import Data.Aeson       (Encoding, ToJSON (..))
import Futurice.Prelude
import Prelude ()
import Web.HttpApiData

-- | Used as type via @DataKinds@
data ItemType = Book | BoardGame
  deriving (Eq, Ord, Show)

type Book      = 'Book
type BoardGame = 'BoardGame

-- | Singleton
data SItemType (ty :: ItemType) where
    SBook      :: SItemType 'Book
    SBoardGame :: SItemType 'BoardGame

-- | Implicit singleton
class    SItemTypeI ty         where sitemType :: SItemType ty
instance SItemTypeI 'Book      where sitemType = SBook
instance SItemTypeI 'BoardGame where sitemType = SBoardGame

-------------------------------------------------------------------------------
-- Some
-------------------------------------------------------------------------------

-- | Existential thing.
data Some f where
    MkSome :: !(f (a :: ItemType)) -> Some f

class ShowP (f :: ItemType -> *) where
    showsPrecP :: Int -> f a -> ShowS

instance ShowP f => Show (Some f) where
    showsPrec d (MkSome fa) = showParen (d > 10)
        $ showString "MkSome "
        . showsPrecP 11 fa

class ToJSONP (f :: ItemType -> *) where
    toJSONP     :: f a -> Value
    toEncodingP :: f a -> Encoding

instance ToJSONP f => ToJSON (Some f) where
    toJSON (MkSome fa)     = toJSONP fa
    toEncoding (MkSome fa) = toEncodingP fa

class FromHttpApiDataP (f :: ItemType -> *) where
    parseUrlPieceP :: Text -> Either Text (Some f)

instance FromHttpApiDataP f => FromHttpApiData (Some f) where
    parseUrlPiece = parseUrlPieceP

class ToHttpApiDataP (f :: ItemType -> *) where
    toUrlPieceP :: f a -> Text

instance ToHttpApiDataP f => ToHttpApiData (Some f) where
    toUrlPiece (MkSome fa) = toUrlPieceP fa

-------------------------------------------------------------------------------
-- Buckets
-------------------------------------------------------------------------------

class HasItemType f where
    itemType :: f ty -> SItemType ty

instance HasItemType SItemType where
    itemType = id

data ItemBuckets f = ItemBuckets
    { bookBucket      :: [f 'Book]
    , boardGameBucket :: [f 'BoardGame]
    }

-- | Fancy 'partitionEithers'.
partitionItems :: HasItemType f => (a -> Some f) -> [a] -> ItemBuckets f
partitionItems g = go where
    go []       = ItemBuckets [] []
    go (x : xs) = case g x of
        MkSome f -> case itemType f of
            SBook      -> ItemBuckets (f : fs0) fs1
            SBoardGame -> ItemBuckets fs0 (f : fs1)
      where
        ~(ItemBuckets fs0 fs1) = go xs
