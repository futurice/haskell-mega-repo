{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Futurice.App.Library.Pages.AddItemPage where

import Futurice.Generics
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types
import Futurice.App.Library.Types.BookInformation

addItemPage :: HtmlPage "additempage"
addItemPage = page_ "Add item to Library" (Just NavAddItem) $ do
    select_ [data_ "futu-id" "add-new-item-select"] $ do
        optionSelected_ True  [ value_ "Book" ] $ "Add book"
        optionSelected_ False  [ value_ "Boardgame" ] $ "Add boardgame"
    form_ [data_ "futu-id" "add-new-book", data_ "form-type" "book", recordAction_ addBookPost, method_ "POST", enctype_ "multipart/form-data"] $ do
        input_ [ name_ "bookinformationid", type_ "hidden"]
        table_ $ do
            tr_ $ do
                th_ "ISBN"
                td_ $ do
                    div_ [ class_ "input-group"] $ do
                        input_ [ class_ "input-group-field", name_ "isbn", id_ "isbn", type_ "text", required_ ""]
                        div_ [ class_ "input-group-button"] $ do
                            button_ [ class_ "button", data_ "futu-id" "find-by-isbn"] $ "Search"
                            button_ [ class_ "button", data_ "futu-id" "clear-add-new-book", style_ "display: none"] $ "Clear"
                    div_ [ id_ "info-box"] ""
            tr_ $ do
                th_ "Title"
                td_ $ input_ [ name_ "title", type_ "text", required_ ""]
            tr_ $ do
                th_ "Author"
                td_ $ input_ [ name_ "author", type_ "text", required_ ""]
            tr_ $ do
                th_ "Publisher"
                td_ $ input_ [ name_ "publisher", type_ "text", required_ ""]
            tr_ $ do
                th_ "Published(year)"
                td_ $ input_ [ name_ "published", type_ "number", required_ ""]
            tr_ $ do
                th_ "Info link"
                td_ $ input_ [ name_ "info-link", type_ "text"]
            tr_ $ do
                th_ "Cover"
                td_ $ do
                    input_ [ name_ "cover-file", type_ "file"]
                    input_ [ name_ "cover-url", type_ "url", style_ "display:none"]
                    img_ [ id_ "cover-image", src_ "", hidden_ ""]
            tr_ $ do
                th_ "Language"
                td_ $ select_ [ name_ "language"] $ do
                  optionSelected_ True [ value_ "-"] $ toHtml ("" :: Text)
                  for_ [minBound .. maxBound] $ \(language :: Language) ->
                    optionSelected_ False [ value_ (enumToText language)] $ toHtml $ enumToText language
            tr_ $ do
                th_ "Category"
                td_ $ select_ [ name_ "category"] $ do
                  optionSelected_ True [ value_ "-"] $ toHtml ("" :: Text)
                  for_ [minBound .. maxBound] $ \(category :: Category) ->
                    optionSelected_ False [ value_ (enumToText category)] $ toHtml $ enumToText category
            tr_ $ do
                th_ "Library"
                td_ $ libraryInput
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
    form_ [data_ "futu-id" "add-new-boardgame",recordAction_ addBoardGamePost, method_ "POST", enctype_ "multipart/form-data", style_ "display: none;"] $ do
        table_ $ do
            tr_ $ do
                th_ "Name"
                td_ $ input_ [ name_ "name", type_ "text", required_ ""]
            tr_ $ do
                th_ "Publisher"
                td_ $ input_ [ name_ "publisher", type_ "text"]
            tr_ $ do
                th_ "Published"
                td_ $ input_ [ name_ "published", type_ "number"]
            tr_ $ do
                th_ "Designer"
                td_ $ input_ [ name_ "designer", type_ "text"]
            tr_ $ do
                th_ "Artist"
                td_ $ input_ [ name_ "artist", type_ "text"]
            tr_ $ do
                th_ "Library"
                td_ $ libraryInput
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
  where
      libraryInput =  ul_ [class_ "no-bullet"] $ for_ usedLibraries $ \lib -> do
          li_ $ do
              input_ [ name_ ("amount-" <> libraryToText lib), type_ "number", min_ "0", style_ "width: 50px; display: inline-block; margin: 0px;"]
              label_ [ style_ "display: inline-block; padding-left: 10px"] (toHtml $ libraryToText lib)
