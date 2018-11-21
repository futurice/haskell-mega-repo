{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Types.Search where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.ItemType

import qualified Data.Text as T

data SortCriteria (ty :: ItemType) where
    BookSort      :: BookSortCriteria -> SortCriteria Book
    BoardGameSort :: BoardGameSortCriteria -> SortCriteria BoardGame

deriving instance Eq (SortCriteria ty)

data BookSortCriteria
    = SortTitle
    | SortAuthor
    | SortPublished
    | SortISBN
  deriving (Eq, Generic, Show, Enum, Bounded)

data BoardGameSortCriteria
    = SortName
    | SortDesigner
  deriving (Eq, Generic, Show, Enum, Bounded)

data SortDirection
    = SortDesc
    | SortAsc
  deriving (Eq, Generic, Show, Enum, Bounded)

deriveGeneric ''BookSortCriteria
deriveGeneric ''BoardGameSortCriteria
deriveGeneric ''SortDirection

instance ToHttpApiData BookSortCriteria where toUrlPiece = enumToText
instance ToHttpApiData BoardGameSortCriteria where toUrlPiece = enumToText
instance ToHttpApiData SortDirection where toUrlPiece = enumToText

instance ToHttpApiDataP SortCriteria where
    toUrlPieceP (BookSort bookSort)           = "book-" <> toUrlPiece bookSort
    toUrlPieceP (BoardGameSort boardgameSort) = "boardgame-" <> toUrlPiece boardgameSort

instance FromHttpApiData BookSortCriteria where parseUrlPiece = enumFromTextE
instance FromHttpApiData BoardGameSortCriteria where parseUrlPiece = enumFromTextE
instance FromHttpApiData SortDirection where parseUrlPiece = enumFromTextE

instance FromHttpApiDataP SortCriteria where
    parseUrlPieceP x = case T.splitOn "-" x of
      ["book",      bookSort]      -> MkSome . BookSort      <$> parseUrlPiece bookSort
      ["boardgame", boardGameSort] -> MkSome . BoardGameSort <$> parseUrlPiece boardGameSort
      _ -> Left "not recognised sort criteria"

instance TextEnum BookSortCriteria where
    type TextEnumNames BookSortCriteria = '["title", "author", "published", "isbn"]
instance TextEnum BoardGameSortCriteria where
    type TextEnumNames BoardGameSortCriteria = '["name", "designer"]
instance TextEnum SortDirection where
    type TextEnumNames SortDirection = '["desc", "asc"]
instance ToParamSchema BookSortCriteria where toParamSchema = enumToParamSchema
instance ToParamSchema SortDirection where toParamSchema = enumToParamSchema
