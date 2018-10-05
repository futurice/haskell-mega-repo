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
          -> Maybe LibraryOrAll
          -> Maybe Text
          -> HtmlPage "indexpage"
indexPage cd direction limit startBookInfoId startBoardGameInfoId search library onlyAvailable = page_ "Library" (Just NavHome) $ do
    fullRow_ $
        div_ [class_ "small button-group", style_ "float: right"] $ do
            a_ [class_ (currentPage Book <> "button"), href_ $ linkToText $ fieldLink indexPageGet (Just $ BookSort SortTitle) Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ "Books"
            a_ [class_ (currentPage BoardGame <> "button"), href_ $ linkToText $ fieldLink indexPageGet (Just $ BoardGameSort SortName) Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ "Boardgames"
    case cd of
      BookCD bookCriteria books -> do
          let bookIndexPageLink = indexPageLink (BookSort bookCriteria)
          paginationLinks cd
          div_ $ do
              form_ [action_ $ linkToText $ fieldLink indexPageGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ do
                  div_ [ class_ "input-group"] $ do
                      input_ [class_ "input-group-field", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search"]
                      div_ [ class_ "input-group-button"] $ button_ [class_ "button", type_ "submit"] "Submit"
                  input_ [hidden_ "", name_ "criteria", value_ (toQueryParam (BookSort bookCriteria))]
                  input_ [hidden_ "", name_ "direction", value_ (toQueryParam direction)]
                  input_ [hidden_ "", name_ "limit", value_ (toQueryParam limit)]
                  for_ startBookInfoId $ \infoid -> input_ [hidden_ "", name_ "start-book", value_ (toQueryParam infoid)]
                  fullRow_ $ do
                      librarySelect
                      onlyAvailableCheckbox
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
              form_ [action_ $ linkToText $ fieldLink indexPageGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ do
                  div_ [ class_ "input-group"] $ do
                      input_ [class_ "input-group-field", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search"]
                      div_ [ class_ "input-group-button"] $ button_ [class_ "button", type_ "submit"] "Submit"
                  input_ [hidden_ "", name_ "criteria", value_ (toQueryParam (BoardGameSort boardgameCriteria))]
                  input_ [hidden_ "", name_ "direction", value_ (toQueryParam direction)]
                  input_ [hidden_ "", name_ "limit", value_ (toQueryParam limit)]
                  for_ startBoardGameInfoId $ \infoid -> input_ [hidden_ "", name_ "start-boardgame", value_ (toQueryParam infoid)]
                  fullRow_ $ do
                      librarySelect
                      -- onlyAvailableCheckbox -- Removed for now as boardgames are not loanable
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
      libraryCompare a = maybe False (\lib -> JustLibrary a == lib) library
      librarySelect =
          select_ [data_ "futu-id" "filter-library-select", name_ "library", style_ "width: 15%; float: left;"] $ do
              optionSelected_ (maybe True (const False) library)  [ value_ "all"] $ "All libraries"
              for_ usedLibraries $ \lib -> optionSelected_ (libraryCompare lib) [ value_ (libraryToText lib)] $ toHtml $ libraryToText lib
      onlyAvailableCheckbox = div_ [class_ "column large-10"] $ do
          case onlyAvailable of
            Just _ -> input_ [type_ "checkbox", name_ "only-available", checked_ ]
            Nothing -> input_ [type_ "checkbox", name_ "only-available"]
          label_ "Show only available books"
      currentPage condition = case cd of
        BookCD _ _ -> if condition == Book then "" else "secondary "
        BoardGameCD _ _ -> if condition == BoardGame then "" else "secondary "
      indexPageLink oldcrit newcrit
          | newcrit /= oldcrit = fieldLink indexPageGet (Just newcrit) (Just SortAsc) (Just limit) Nothing Nothing search library onlyAvailable
          | otherwise          = fieldLink indexPageGet (Just newcrit) (Just $ reverseDir direction) (Just limit) Nothing Nothing search library onlyAvailable
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
          unless (null infos) $ a_ [class_ "button", href_ $ linkToText $ fieldLink indexPageGet (Just $ BookSort criteria) (Just direction) (Just limit) (lastBookInfoId infos) Nothing search library onlyAvailable] $ toHtml ("next" :: Text)
      paginationLinks (BoardGameCD criteria infos) = do
          for_ startBoardGameInfoId $ \_ -> a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          unless (null infos) $ a_ [class_ "button", href_ $ linkToText $ fieldLink indexPageGet (Just $ BoardGameSort criteria) (Just direction) (Just limit) Nothing (lastBoardGameInfoId infos) search library onlyAvailable] $ toHtml ("next" :: Text)
