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

type LibraryAPI = IndexPageEndpoint

type IndexPageEndpoint =
    SSOUser :> Get '[HTML] (HtmlPage "indexpage")
    :<|> "book" :> Get '[JSON] [BookInformationResponse]
    :<|> "book" :> Capture "id" LoanableId :> Get '[JSON] [BookInformationResponse]
    :<|> "book" :> "cover" :> Capture "picture" Text :> Get '[PNG] (Headers '[Header "Cache-Control" Text] (DynamicImage))
    :<|> "loan" :> Get '[JSON] [Loan]

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy
