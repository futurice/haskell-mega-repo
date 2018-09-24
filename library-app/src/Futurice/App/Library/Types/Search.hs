{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Types.Search where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation

import qualified Data.Text as T

data CriteriaAndData where
    BookCD :: BookSortCriteria -> [BookInformation] -> CriteriaAndData
    BoardGameCD :: BoardGameSortCriteria -> [BoardGameInformation] -> CriteriaAndData

data SortCriteriaAndStart where
    BookCS :: BookSortCriteria -> Maybe BookInformation -> SortCriteriaAndStart
    BoardGameCS :: BoardGameSortCriteria -> Maybe BoardGameInformation -> SortCriteriaAndStart

data SortCriteria = BookSort BookSortCriteria
                  | BoardGameSort BoardGameSortCriteria
                  deriving (Eq, Generic)

data BookSortCriteria = SortTitle
                      | SortAuthor
                      | SortPublished
                      | SortISBN
                      deriving (Eq, Generic, Show, Enum, Bounded)

data BoardGameSortCriteria = SortName
                           | SortDesigner
                           deriving (Eq, Generic, Show, Enum, Bounded)

data SortDirection = SortDesc
                   | SortAsc
                   deriving (Eq, Generic, Show, Enum, Bounded)

deriveGeneric ''BookSortCriteria
deriveGeneric ''BoardGameSortCriteria
deriveGeneric ''SortDirection

instance ToHttpApiData BookSortCriteria where toUrlPiece = enumToText
instance ToHttpApiData BoardGameSortCriteria where toUrlPiece = enumToText
instance ToHttpApiData SortDirection where toUrlPiece = enumToText
instance ToHttpApiData SortCriteria where
    toUrlPiece (BookSort bookSort) = "book-" <> toUrlPiece bookSort
    toUrlPiece (BoardGameSort boardgameSort) = "boardgame-" <> toUrlPiece boardgameSort
instance FromHttpApiData BookSortCriteria where parseUrlPiece = enumFromTextE
instance FromHttpApiData BoardGameSortCriteria where parseUrlPiece = enumFromTextE
instance FromHttpApiData SortDirection where parseUrlPiece = enumFromTextE
instance FromHttpApiData SortCriteria where
    parseUrlPiece x = case T.splitOn "-" x of
      ["book", bookSort] -> BookSort <$> parseUrlPiece bookSort
      ["boardgame", boardGameSort] -> BoardGameSort <$> parseUrlPiece boardGameSort
      _ -> Left "not recognised sort criteria"

instance TextEnum BookSortCriteria where
    type TextEnumNames BookSortCriteria = '["title", "author", "published", "isbn"]
instance TextEnum BoardGameSortCriteria where
    type TextEnumNames BoardGameSortCriteria = '["name", "designer"]
instance TextEnum SortDirection where
    type TextEnumNames SortDirection = '["desc", "asc"]
instance ToParamSchema BookSortCriteria where toParamSchema = enumToParamSchema
instance ToParamSchema SortDirection where toParamSchema = enumToParamSchema
