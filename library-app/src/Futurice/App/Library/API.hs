{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Library.API where

import Futurice.Prelude
import Prelude ()

import Codec.Picture             (DynamicImage)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)
import Servant.JuicyPixels       (PNG)
import Servant.Multipart

import Futurice.App.Library.Types

----------------------------
-- Machine API
----------------------------

data Record route = Record
    { booksGet             :: route :- "book" :> Get '[JSON] [BookInformationResponse]
    , bookGet              :: route :- "book" :> Capture "id" BookInformationId :> Get '[JSON] BookInformationResponse
    , bookCoverGet         :: route :- BookCoverEndpoint
    , borrowPost           :: route :- SSOUser :> "book" :> "borrow" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    , snatchPost           :: route :- SSOUser :> "book" :> "snatch" :> Capture "id" ItemId :> Post '[JSON] Loan
    , loansGet             :: route :- "loan" :> Get '[JSON] [Loan]
    , loanGet              :: route :- "loan" :> Capture "id" LoanId :> Get '[JSON] Loan
    , returnPost           :: route :- "return" :> Capture "id" LoanId :> Post '[JSON] Bool
    , personalLoansGet     :: route :- SSOUser :> "user" :> "loan" :> Get '[JSON] [Loan]
    } deriving (Generic)

type LibraryAPI = ToServantApi Record

libraryApi :: Proxy LibraryAPI
libraryApi = genericApi (Proxy :: Proxy Record)

----------------------------
-- Page API
----------------------------

data HtmlRecord route = HtmlRecord
    { addItemPageGet       :: route :- "item" :> "add" :> Get '[HTML] (HtmlPage "additempage")
    , addBookPost          :: route :- "item" :> "add" :> "book" :> MultipartForm Mem AddBookInformation :> Post '[HTML] (HtmlPage "additempage")
    , addBoardGamePost     :: route :- "item" :> "add" :> "boardgame" :> MultipartForm Mem AddBoardGameInformation :> Post '[HTML] (HtmlPage "additempage")
    , bookPageGet          :: route :- BookInformationPageEndpoint
    , indexPageGet         :: route :- IndexPageEndpoint
    , personalLoansPageGet :: route :- SSOUser :> "user" :> "page" :> Get '[HTML] (HtmlPage "personalinformation")
    } deriving (Generic)

type IndexPageEndpoint = QueryParam "criteria" SortCriteria
                         :> QueryParam "direction" SortDirection
                         :> QueryParam "limit" Int
                         :> QueryParam "start" BookInformationId
                         :> QueryParam "search" Text
                         :> Get '[HTML] (HtmlPage "indexpage")
type BookCoverEndpoint = "book" :> "cover" :> Capture "picture" Text :> Get '[PNG] (Headers '[Header "Cache-Control" Text] (DynamicImage))
type BookInformationPageEndpoint = "book" :> "page" :> Capture "id" BookInformationId :> Get '[HTML] (HtmlPage "bookinformation")

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)
