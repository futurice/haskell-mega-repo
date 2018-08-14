{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Types.Search where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data SortCriteria = SortTitle
                  | SortAuthor
                  | SortPublished
                  | SortISBN
                  deriving (Eq, Generic, Show, Enum, Bounded)

data SortDirection = SortDesc
                   | SortAsc
                   deriving (Eq, Generic, Show, Enum, Bounded)

deriveGeneric ''SortCriteria
deriveGeneric ''SortDirection

instance ToHttpApiData SortCriteria where toUrlPiece = enumToText
instance ToHttpApiData SortDirection where toUrlPiece = enumToText
instance FromHttpApiData SortCriteria where parseUrlPiece = enumFromTextE
instance FromHttpApiData SortDirection where parseUrlPiece = enumFromTextE

instance TextEnum SortCriteria where
    type TextEnumNames SortCriteria = '["title", "author", "published", "isbn"]
instance TextEnum SortDirection where
    type TextEnumNames SortDirection = '["desc", "asc"]
instance ToParamSchema SortCriteria where toParamSchema = enumToParamSchema
instance ToParamSchema SortDirection where toParamSchema = enumToParamSchema
