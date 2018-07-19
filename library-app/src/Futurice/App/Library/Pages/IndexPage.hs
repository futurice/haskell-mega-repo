{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.IndexPage where

import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

indexPage :: [BookInformationResponse] -> HtmlPage "indexpage"
indexPage books = page_ "Library" (Just NavHome) $ do
    fullRow_ $ sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Cover"
            th_ "Title"
            th_ "Author"
            th_ "Published"
            th_ "ISBN"
        tbody_ $ for_ (take 40 books) $ \(BookInformationResponse binfoid title isbn author _publisher published cover _amazonLink _books) -> tr_ $ do
            td_ $ img_ [src_ $ toUrlPiece $ fieldLink bookCoverGet cover ]
            td_ $ a_ [href_ $ linkToText $ fieldLink bookPageGet binfoid] $ toHtml title
            td_ $ toHtml $ author
            td_ $ toHtml $ show published
            td_ $ toHtml $ isbn
