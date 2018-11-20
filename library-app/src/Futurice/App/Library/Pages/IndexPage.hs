{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.IndexPage where

import Control.Lens              (_last)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

indexPage
    :: forall ty.
       SortCriteria ty
    -> [ItemInfo ty]
    -> SortDirection
    -> Int
    -> Maybe BookInformationId
    -> Maybe BoardGameInformationId
    -> Maybe Text
    -> Maybe LibraryOrAll
    -> Maybe Text
    -> HtmlPage "indexpage"
indexPage crit itemInfos direction limit startBookInfoId startBoardGameInfoId search library onlyAvailable = page_ "Library" (Just NavHome) $ do
    fullRow_ $
        div_ [class_ "small button-group", style_ "float: right"] $ do
            a_ [class_ (currentPage Book      <> "button"), recordHref_ indexPageGet (Just $ MkSome $ BookSort SortTitle)     Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ "Books"
            a_ [class_ (currentPage BoardGame <> "button"), recordHref_ indexPageGet (Just $ MkSome $ BoardGameSort SortName) Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ "Boardgames"
    case crit of
      BookSort bookCriteria | books <- map fromItemBook itemInfos -> do
          paginationLinks crit itemInfos
          div_ $ do
              form_ [ recordAction_ indexPageGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ do
                  row_ $ do
                      onlyAvailableCheckbox
                      div_ [ class_ "columns large-2", style_ "padding-right: 0px;"] $ librarySelect
                      div_ [ class_ "columns large-8", style_ "padding-left: 0px;"] $ div_ [ class_ "input-group"] $ do
                          input_ [class_ "input-group-field", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search"]
                          div_ [ class_ "input-group-button"] $ button_ [class_ "button", type_ "submit"] "Submit"
                  input_ [hidden_ "", name_ "criteria",  value_ $ toQueryParam bookCriteria ]
                  input_ [hidden_ "", name_ "direction", value_ $ toQueryParam direction ]
                  input_ [hidden_ "", name_ "limit",     value_ $ toQueryParam limit ]
                  for_ startBookInfoId $ \infoid -> input_ [hidden_ "", name_ "start-book", value_ (toQueryParam infoid)]
          fullRow_ $ table_ [id_ "main"] $ do
              thead_ $ tr_ $ do
                  th_ "Cover"
                  th_ $ a_ [ indexPageLink (BookSort SortTitle)] "Title"
                  th_ $ a_ [ indexPageLink (BookSort SortAuthor)] "Author"
                  th_ $ a_ [ indexPageLink (BookSort SortPublished)] "Published"
                  th_ $ a_ [ indexPageLink (BookSort SortISBN)] "ISBN"
              tbody_ $ for_ books $ \(BookInformation binfoid title isbn author _publisher published cover _amazonLink) -> tr_ $ do
                  td_ $ img_ [height_ "160", width_ "128", src_ $ toUrlPiece $ fieldLink bookCoverGet cover ]
                  td_ $ a_ [href_ $ linkToText $ fieldLink bookPageGet binfoid] $ toHtml title
                  td_ $ toHtml $ author
                  td_ $ toHtml $ show published
                  td_ $ toHtml $ isbn
          paginationLinks crit itemInfos
      BoardGameSort boardgameCriteria | boardgames <- map fromItemBoardGame itemInfos -> do
          paginationLinks crit itemInfos
          div_ $ do
              form_ [action_ $ linkToText $ fieldLink indexPageGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing] $ do
                  row_ $ do
                      div_ [ class_ "columns large-2"] $ librarySelect
                      div_ [ class_ "columns large-10"] $ div_ [ class_ "input-group"] $ do
                          input_ [class_ "input-group-field", size_ "50", type_ "text", id_ "search-box", placeholder_ "Search...", value_ $ fromMaybe "" search, name_ "search"]
                          div_ [ class_ "input-group-button"] $ button_ [class_ "button", type_ "submit"] "Submit"
                  input_ [hidden_ "", name_ "criteria",  value_ $ toQueryParam boardgameCriteria ]
                  input_ [hidden_ "", name_ "direction", value_ $ toQueryParam direction ]
                  input_ [hidden_ "", name_ "limit",     value_ $ toQueryParam limit ]
                  for_ startBoardGameInfoId $ \infoid -> input_ [hidden_ "", name_ "start-boardgame", value_ (toQueryParam infoid)]
          fullRow_ $ table_ [id_ "main"] $ do
              thead_ $ tr_ $ do
                  th_ $ a_ [ indexPageLink (BoardGameSort SortName)] "Name"
                  th_ $ a_ [ indexPageLink (BoardGameSort SortDesigner)] "Designer"
                  th_ $ "Published"
              tbody_ $ for_ boardgames $ \(BoardGameInformation binfoid name _publisher published designer _artist) -> tr_ $ do
                  td_ $ a_ [href_ $ linkToText $ fieldLink boardGamePageGet binfoid] $ toHtml name
                  td_ $ toHtml $ fromMaybe "" designer
                  td_ $ toHtml $ maybe "" show published
          paginationLinks crit itemInfos

  where
      libraryCompare a = maybe False (\lib -> JustLibrary a == lib) library

      librarySelect :: Html ()
      librarySelect =
          select_ [data_ "futu-id" "filter-library-select", name_ "library", style_ "float: left;"] $ do
              optionSelected_ (maybe True (const False) library)  [ value_ "all"] $ "All libraries"
              for_ usedLibraries $ \lib -> optionSelected_ (libraryCompare lib) [ value_ (libraryToText lib)] $ toHtml $ libraryToText lib

      onlyAvailableCheckbox :: Html ()
      onlyAvailableCheckbox = div_ [class_ "column large-2"] $ do
          div_ [ class_ "column large-8 small-2", style_ "padding: 0px;"] $ span_ "Show only available items"
          div_ [ class_ "column large-4 small-4", style_ "padding-left: 0px; float: left;"] $ div_ [ class_ "switch large"] $ do
              case onlyAvailable of
                Just _ -> input_ [class_ "switch-input", type_ "checkbox", name_ "only-available", id_ "testi", checked_ ]
                Nothing -> input_ [class_ "switch-input", type_ "checkbox", name_ "only-available", id_ "testi"]
              label_ [ class_ "switch-paddle", attrfor_ "testi"] $ do
                  span_ [ class_ "switch-active"] $ "Yes"
                  span_ [ class_ "switch-inactive"] $ "No"

      currentPage condition = case crit of
          BookSort _      -> if condition == Book then "" else "secondary "
          BoardGameSort _ -> if condition == BoardGame then "" else "secondary "

      indexPageLink :: SortCriteria ty -> Attribute
      indexPageLink newcrit
          | newcrit /= crit = recordHref_ indexPageGet (Just (MkSome newcrit)) (Just SortAsc) (Just limit) Nothing Nothing search library onlyAvailable
          | otherwise       = recordHref_ indexPageGet (Just (MkSome newcrit)) (Just $ reverseDir direction) (Just limit) Nothing Nothing search library onlyAvailable

      reverseDir SortDesc = SortAsc
      reverseDir SortAsc  = SortDesc

      lastBookInfoId infos =
          infos ^? _last . getter fromItemBook . bookInformationId
      lastBoardGameInfoId infos =
          infos ^? _last . getter fromItemBoardGame . boardGameInformationId

      paginationLinks :: SortCriteria ty -> [ItemInfo ty] -> HtmlT Identity ()
      paginationLinks (BookSort criteria) infos = do
          for_ startBookInfoId $ \_ ->
              a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          unless (null infos) $
              a_ [class_ "button", recordHref_ indexPageGet (Just $ MkSome $ BookSort criteria) (Just direction) (Just limit) (lastBookInfoId infos) Nothing search library onlyAvailable] $ toHtml ("next" :: Text)
      paginationLinks (BoardGameSort criteria) infos = do
          for_ startBoardGameInfoId $ \_ ->
              a_ [class_ "button", href_ "javascript:history.back()"] $ toHtml ("prev" :: Text)
          unless (null infos) $
              a_ [class_ "button", recordHref_ indexPageGet (Just $ MkSome $ BoardGameSort criteria) (Just direction) (Just limit) Nothing (lastBoardGameInfoId infos) search library onlyAvailable] $ toHtml ("next" :: Text)

fromItemBook :: ItemInfo 'Book -> BookInformation
fromItemBook (ItemBook bookInfo) = bookInfo

fromItemBoardGame :: ItemInfo 'BoardGame -> BoardGameInformation
fromItemBoardGame (ItemBoardGame bookInfo) = bookInfo
