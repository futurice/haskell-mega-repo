{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Library.API where

import Futurice.Prelude
import Prelude ()

import Codec.Picture             (DynamicImage)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.HTML.Lucid        (HTML)
import Servant.JuicyPixels       (PNG)

import Futurice.App.Library.Types

type LibraryAPI =
    Get '[HTML] (HtmlPage "indexpage")
    :<|> "book" :> Get '[JSON] [BookInformationResponse]
    :<|> "book" :> Capture "id" BookInformationId :> Get '[JSON] BookInformationResponse
    :<|> BookInformationPageEndpoint
    :<|> BookCoverEndpoint
    :<|> SSOUser :> "book" :> "borrow" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    :<|> SSOUser :> "book" :> "snatch" :> ReqBody '[JSON] BorrowRequest :> Post '[JSON] Loan
    :<|> "loan" :> Get '[JSON] [Loan]
    :<|> "loan" :> Capture "id" LoanId :> Get '[JSON] Loan

type BookCoverEndpoint = "book" :> "cover" :> Capture "picture" Text :> Get '[PNG] (Headers '[Header "Cache-Control" Text] (DynamicImage))
type BookInformationPageEndpoint = "book" :> "page" :> Capture "id" BookInformationId :> Get '[HTML] (HtmlPage "bookinformation")

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy

bookCoverEndpoint :: Proxy BookCoverEndpoint
bookCoverEndpoint = Proxy

bookInformationPageEndpoint :: Proxy BookInformationPageEndpoint
bookInformationPageEndpoint = Proxy
