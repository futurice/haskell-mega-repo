{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Library.API where

import Futurice.Prelude
import Prelude ()

import Codec.Picture               (DynamicImage)
import Futurice.App.Sisosota.Types (ContentHash)
import Futurice.Lucid.Foundation   (HtmlPage)
import Futurice.Servant            (SSOUser)
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid          (HTML)
import Servant.JuicyPixels         (PNG)
import Servant.Multipart

import Futurice.App.Library.Types

----------------------------
-- Machine API
----------------------------

data Record route = Record
    { booksGet             :: route :- "book" :> Get '[JSON] [BookInformationResponse]
    , bookGet              :: route :- "book" :> Capture "id" BookInformationId :> Get '[JSON] BookInformationResponse
    , bookByISBNGet        :: route :- "book" :> "isbn" :> Capture "isbn" Text :> Get '[JSON] BookInformationByISBNResponse
    , bookCoverGet         :: route :- BookCoverEndpoint
    , borrowPost           :: route :- SSOUser :> "book" :> "borrow" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    , itemDelete           :: route :- "item"  :> Capture "id" ItemId :> Delete '[JSON] Text
    , snatchPost           :: route :- SSOUser :> "book" :> "snatch" :> Capture "id" ItemId :> Post '[JSON] Loan
    , loansGet             :: route :- "loan" :> Get '[JSON] [Loan]
    , loanGet              :: route :- "loan" :> Capture "id" LoanId :> Get '[JSON] Loan
    , returnPost           :: route :- "return" :> Capture "id" LoanId :> Post '[JSON] Bool
    , personalLoansGet     :: route :- SSOUser :> "user" :> "loan" :> Get '[JSON] [Loan]
    , sendReminderEmails   :: route :- SSOUser :> "remainder" :> Get '[JSON] Bool
    } deriving (Generic)

type LibraryAPI = ToServantApi Record

libraryApi :: Proxy LibraryAPI
libraryApi = genericApi (Proxy :: Proxy Record)

----------------------------
-- Page API
----------------------------

data HtmlRecord route = HtmlRecord
    { addBookPost          :: route :- "item" :> "add" :> "book" :> MultipartForm Mem AddBookInformation :> Post '[HTML] (HtmlPage "additempage")
    , addBoardGamePost     :: route :- "item" :> "add" :> "boardgame" :> MultipartForm Mem AddBoardGameInformation :> Post '[HTML] (HtmlPage "additempage")
    , addItemPageGet       :: route :- "item" :> "add" :> Get '[HTML] (HtmlPage "additempage")
    , addItemPost          :: route :- "item"  :> MultipartForm Mem AddItemRequest :> Post '[HTML] (HtmlPage "edititempage")
    , bookPageGet          :: route :- BookInformationPageEndpoint
    , boardGamePageGet     :: route :- BoardGameInformationPageEndpoint
    , editBoardGamePageGet :: route :- "item" :> "edit" :> "boardgame" :> Capture "id" BoardGameInformationId :> Get '[HTML] (HtmlPage "edititempage")
    , editBoardGamePost    :: route :- "item" :> "edit" :> "boardgame" :> MultipartForm Mem EditBoardGameInformation :> Post '[HTML] (HtmlPage "boardgameinformation")
    , editBookPageGet      :: route :- "item" :> "edit" :> "book" :> Capture "id" BookInformationId :> Get '[HTML] (HtmlPage "edititempage")
    , editBookPost         :: route :- "item" :> "edit" :> "book" :> MultipartForm Mem EditBookInformation :> Post '[HTML] (HtmlPage "bookinformation")
    , indexPageGet         :: route :- IndexPageEndpoint
    , personalLoansPageGet :: route :- SSOUser :> "user" :> "page" :> Get '[HTML] (HtmlPage "personalinformation")
    } deriving (Generic)

type IndexPageEndpoint = QueryParam "criteria" SortCriteria
                         :> QueryParam "direction" SortDirection
                         :> QueryParam "limit" Int
                         :> QueryParam "start-book" BookInformationId
                         :> QueryParam "start-boardgame" BoardGameInformationId
                         :> QueryParam "search" Text
                         :> QueryParam "library" LibraryOrAll
                         :> QueryParam "only-available" Text
                         :> Get '[HTML] (HtmlPage "indexpage")
type BookCoverEndpoint = "book" :> "cover" :> Capture "picture" ContentHash :> Get '[PNG] (Headers '[Header "Cache-Control" Text] (DynamicImage))
type BookInformationPageEndpoint = "book" :> "page" :> Capture "id" BookInformationId :> Get '[HTML] (HtmlPage "bookinformation")
type BoardGameInformationPageEndpoint = "boardgame" :> "page" :> Capture "id" BoardGameInformationId :> Get '[HTML] (HtmlPage "boardgameinformation")

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)
