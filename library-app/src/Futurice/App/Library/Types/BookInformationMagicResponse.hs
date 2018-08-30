{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformationMagicResponse where

import Data.Aeson
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data DataSource = DSDatabase
                | DSAmazon
                deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)

data BooksPerLibrary = BooksPerLibrary
    { _bookLibrary   :: !Library
    , _amountOfBooks :: !Int
    }
    deriving (Eq, Ord, Show, Generic, ToSchema, Typeable)

data BookInformationMagicResponse = BookInformationMagicResponse
    { _idM              :: !BookInformationId
    , _titleM           :: !Text
    , _ISBNM            :: !Text
    , _authorM          :: !Text
    , _publisherM       :: !Text
    , _publishedM       :: !Int
    , _coverM           :: !Text
    , _amazonLinkM      :: !Text
    , _booksM           :: ![BooksPerLibrary]
    , _dataSourceM      :: !DataSource
    } deriving  (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''BooksPerLibrary
deriveGeneric ''BookInformationMagicResponse

instance ToJSON BooksPerLibrary where
    toJSON (BooksPerLibrary lib n) = object ["library" .= libraryToText lib, "amountOfBooks" .= n]

deriveVia [t| ToJSON BookInformationMagicResponse `Via` Sopica BookInformationMagicResponse |]

instance ToSchema BookInformationMagicResponse
