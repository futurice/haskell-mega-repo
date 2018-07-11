{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.BookInformationPage where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

bookInformationPage :: BookInformationResponse -> HtmlPage ("bookinformation")
bookInformationPage (BookInformationResponse _id title isbn author _publisher published cover _amazonLink _books) = page_ "Book details " pageParams $ do
    navigation_ Nothing
    div_ [] $ do
        img_ [src_ $ linkToText $ fieldLink bookCoverGet cover ]
    div_ [] $ do
        table_ $ do
            tr_ $ do
                th_ "Title"
                td_ $ toHtml $ title
            tr_ $ do
                th_ "Author"
                td_ $ toHtml $ author
            tr_ $ do
                th_ "Published"
                td_ $ toHtml $ show published
            tr_ $ do
                th_ "ISBN"
                td_ $ toHtml $ isbn
