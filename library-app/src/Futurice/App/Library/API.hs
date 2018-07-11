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

import Futurice.App.Library.Types

data Record route = Record
    { indexPageGet     :: route :- Get '[HTML] (HtmlPage "indexpage")
    , booksGet         :: route :- "book" :> Get '[JSON] [BookInformationResponse]
    , bookGet          :: route :- "book" :> Capture "id" BookInformationId :> Get '[JSON] BookInformationResponse
    , bookPageGet      :: route :- BookInformationPageEndpoint
    , bookCoverGet     :: route :- BookCoverEndpoint
    , borrowPost       :: route :- SSOUser :> "book" :> "borrow" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    , snatchPost       :: route :- SSOUser :> "book" :> "snatch" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    , loansGet         :: route :- "loan" :> Get '[JSON] [Loan]
    , loanGet          :: route :- "loan" :> Capture "id" LoanId :> Get '[JSON] Loan
    } deriving (Generic)

type LibraryAPI = ToServantApi Record

type BookCoverEndpoint = "book" :> "cover" :> Capture "picture" Text :> Get '[PNG] (Headers '[Header "Cache-Control" Text] (DynamicImage))
type BookInformationPageEndpoint = "book" :> "page" :> Capture "id" BookInformationId :> Get '[HTML] (HtmlPage "bookinformation")

libraryApi :: Proxy LibraryAPI
libraryApi = genericApi (Proxy :: Proxy Record)
