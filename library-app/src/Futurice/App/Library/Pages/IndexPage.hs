{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.IndexPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

indexPage :: [BookInformationResponse]
          -> SortCriteria
          -> SortDirection
          -> Int
          -> Maybe BookInformationId
          -> Maybe Text
          -> HtmlPage "indexpage"
indexPage books criteria direction limit startBookInfoId search = page_ "Library" (Just NavHome) $ do
    div_ $ do
        paginationLinks
        form_ [action_ $ linkToText $ fieldLink indexPageGet (Just criteria) (Just direction) (Just limit) startBookInfoId Nothing] $ do
            button_ [class_ "button", style_ "float: right"] "Submit"
            input_ [style_ "width: 90%;", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search", required_ ""]
    fullRow_ $ table_ [id_ "main"] $ do
        thead_ $ tr_ $ do
            th_ "Cover"
            th_ $ a_ [href_ $ linkToText $ indexPageLink SortTitle] "Title"
            th_ $ a_ [href_ $ linkToText $ indexPageLink SortAuthor] "Author"
            th_ $ a_ [href_ $ linkToText $ indexPageLink SortPublished] "Published"
            th_ $ a_ [href_ $ linkToText $ indexPageLink SortISBN] "ISBN"
        tbody_ $ for_ books $ \(BookInformationResponse binfoid title isbn author _publisher published cover _amazonLink _books) -> tr_ $ do
            td_ $ img_ [height_ "160", width_ "128", src_ $ toUrlPiece $ fieldLink bookCoverGet cover ]
            td_ $ a_ [href_ $ linkToText $ fieldLink bookPageGet binfoid] $ toHtml title
            td_ $ toHtml $ author
            td_ $ toHtml $ show published
            td_ $ toHtml $ isbn
    paginationLinks
  where
      indexPageLink newcrit
          | newcrit /= criteria = fieldLink indexPageGet (Just newcrit) (Just SortAsc) (Just limit) Nothing search
          | otherwise           = fieldLink indexPageGet (Just newcrit) (Just $ reverseDir direction) (Just limit) Nothing search
      reverseDir SortDesc = SortAsc
      reverseDir SortAsc = SortDesc
      lastBookInfoId = case listToMaybe books of
        Just _ -> Just $ _id $ last books
        Nothing -> Nothing
      paginationLinks = do
          for_ startBookInfoId $ \_ -> a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          for_ (listToMaybe books) $ \_ -> a_ [class_ "button", href_ $ linkToText $ fieldLink indexPageGet (Just criteria) (Just direction) (Just limit) lastBookInfoId search] $ toHtml ("next" :: Text)
