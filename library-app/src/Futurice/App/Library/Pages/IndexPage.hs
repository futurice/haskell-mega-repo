{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.IndexPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

data Pages = Book
           | BoardGame
           deriving Eq

indexPage :: CriteriaAndData
          -> SortDirection
          -> Int
          -> Maybe BookInformationId
          -> Maybe BoardGameInformationId
          -> Maybe Text
          -> HtmlPage "indexpage"
indexPage cd direction limit startBookInfoId startBoardGameInfoId search = page_ "Library" (Just NavHome) $ do
    fullRow_ $
        div_ [class_ "small button-group", style_ "float: right"] $ do
            a_ [class_ (currentPage Book <> "button"), href_ $ linkToText $ fieldLink indexPageGet (Just $ BookSort SortTitle) Nothing Nothing Nothing Nothing Nothing] $ "Books"
            a_ [class_ (currentPage BoardGame <> "button"), href_ $ linkToText $ fieldLink indexPageGet (Just $ BoardGameSort SortName) Nothing Nothing Nothing Nothing Nothing] $ "Boardgames"
    case cd of
      BookCD bookCriteria books -> do
          let bookIndexPageLink = indexPageLink (BookSort bookCriteria)
          paginationLinks cd
          div_ $ do
              form_ [action_ $ linkToText $ fieldLink indexPageGet (Just $ BookSort bookCriteria) (Just direction) (Just limit) startBookInfoId Nothing Nothing] $ do
                  button_ [class_ "button", style_ "float: right"] "Submit"
                  input_ [style_ "width: 90%;", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search", required_ ""]
                  input_ [hidden_ "", name_ "criteria", value_ (toQueryParam (BookSort bookCriteria))]
                  input_ [hidden_ "", name_ "direction", value_ (toQueryParam direction)]
                  input_ [hidden_ "", name_ "limit", value_ (toQueryParam limit)]
                  for_ startBookInfoId $ \infoid -> input_ [hidden_ "", name_ "start-book", value_ (toQueryParam infoid)]
          fullRow_ $ table_ [id_ "main"] $ do
              thead_ $ tr_ $ do
                  th_ "Cover"
                  th_ $ a_ [href_ $ linkToText $ bookIndexPageLink (BookSort SortTitle)] "Title"
                  th_ $ a_ [href_ $ linkToText $ bookIndexPageLink (BookSort SortAuthor)] "Author"
                  th_ $ a_ [href_ $ linkToText $ bookIndexPageLink (BookSort SortPublished)] "Published"
                  th_ $ a_ [href_ $ linkToText $ bookIndexPageLink (BookSort SortISBN)] "ISBN"
              tbody_ $ for_ books $ \(BookInformation binfoid title isbn author _publisher published cover _amazonLink) -> tr_ $ do
                  td_ $ img_ [height_ "160", width_ "128", src_ $ toUrlPiece $ fieldLink bookCoverGet cover ]
                  td_ $ a_ [href_ $ linkToText $ fieldLink bookPageGet binfoid] $ toHtml title
                  td_ $ toHtml $ author
                  td_ $ toHtml $ show published
                  td_ $ toHtml $ isbn
          paginationLinks cd
      BoardGameCD boardgameCriteria boardgames -> do
          let boardgameIndexPageLink = indexPageLink (BoardGameSort boardgameCriteria)
          paginationLinks cd
          div_ $ do
              form_ [action_ $ linkToText $ fieldLink indexPageGet (Just $ BoardGameSort boardgameCriteria) (Just direction) (Just limit) Nothing startBoardGameInfoId Nothing] $ do
                  button_ [class_ "button", style_ "float: right"] "Submit"
                  input_ [style_ "width: 90%;", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search", required_ ""]
                  input_ [hidden_ "", name_ "criteria", value_ (toQueryParam (BoardGameSort boardgameCriteria))]
                  input_ [hidden_ "", name_ "direction", value_ (toQueryParam direction)]
                  input_ [hidden_ "", name_ "limit", value_ (toQueryParam limit)]
                  for_ startBoardGameInfoId $ \infoid -> input_ [hidden_ "", name_ "start-boardgame", value_ (toQueryParam infoid)]
          fullRow_ $ table_ [id_ "main"] $ do
              thead_ $ tr_ $ do
                  th_ $ a_ [href_ $ linkToText $ boardgameIndexPageLink (BoardGameSort SortName)] "Name"
                  th_ $ a_ [href_ $ linkToText $ boardgameIndexPageLink (BoardGameSort SortDesigner)] "Designer"
                  th_ $ "Published"
              tbody_ $ for_ boardgames $ \(BoardGameInformation binfoid name _publisher published designer _artist) -> tr_ $ do
                  td_ $ a_ [href_ $ linkToText $ fieldLink boardGamePageGet binfoid] $ toHtml name
                  td_ $ toHtml $ fromMaybe "" designer
                  td_ $ toHtml $ maybe "" show published
          paginationLinks cd

  where
      currentPage condition = case cd of
        BookCD _ _ -> if condition == Book then "" else "secondary "
        BoardGameCD _ _ -> if condition == BoardGame then "" else "secondary "
      indexPageLink oldcrit newcrit
          | newcrit /= oldcrit = fieldLink indexPageGet (Just newcrit) (Just SortAsc) (Just limit) Nothing Nothing search
          | otherwise          = fieldLink indexPageGet (Just newcrit) (Just $ reverseDir direction) (Just limit) Nothing Nothing search
      reverseDir SortDesc = SortAsc
      reverseDir SortAsc = SortDesc
      lastBookInfoId infos = case listToMaybe infos of
        Just _ -> Just $ _bookInformationId $ last infos
        Nothing -> Nothing
      lastBoardGameInfoId infos = case listToMaybe infos of
        Just _ -> Just $ last infos ^. boardGameInformationId
        Nothing -> Nothing
      paginationLinks :: CriteriaAndData -> HtmlT Identity ()
      paginationLinks (BookCD criteria infos) = do
          for_ startBookInfoId $ \_ -> a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          for_ (listToMaybe infos) $ \_ -> a_ [class_ "button", href_ $ linkToText $ fieldLink indexPageGet (Just $ BookSort criteria) (Just direction) (Just limit) (lastBookInfoId infos) Nothing search] $ toHtml ("next" :: Text)
      paginationLinks (BoardGameCD criteria infos) = do
          for_ startBoardGameInfoId $ \_ -> a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          for_ (listToMaybe infos) $ \_ -> a_ [class_ "button", href_ $ linkToText $ fieldLink indexPageGet (Just $ BoardGameSort criteria) (Just direction) (Just limit) Nothing (lastBoardGameInfoId infos) search] $ toHtml ("next" :: Text)
