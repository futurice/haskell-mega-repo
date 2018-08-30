{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types (
    AddBookInformation (..),
    AddBoardGameInformation (..),
    BoardGameInformation (..),
    bookInformationId,
    bookTitle,
    bookISBN,
    bookAuthor,
    bookPublisher,
    bookPublished,
    bookCover,
    bookAmazonLink,
    BoardGameInformationId,
    BookInformation (..),
    BookInformationId,
    BookInformationResponse (..),
    BookInformationMagicResponse (..),
    BooksPerLibrary (..),
    DataSource (..),
    Books (..),
    BorrowRequest (..),
    Loan (..),
    loanId,
    loanInformation,
    Item (..),
    itemInfo,
    ItemId,
    ItemInfo (..),
    LoanId,
    Library (..),
    allLibraries,
    libraryToText,
    usedLibraries,
    SortCriteria (..),
    SortDirection (..)
    ) where

import Futurice.App.Library.Types.AddItemRequest
import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.BookInformationMagicResponse
import Futurice.App.Library.Types.BookInformationResponse
import Futurice.App.Library.Types.BorrowRequest
import Futurice.App.Library.Types.Item
import Futurice.App.Library.Types.Library
import Futurice.App.Library.Types.Loan
import Futurice.App.Library.Types.Search
