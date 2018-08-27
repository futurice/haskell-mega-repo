{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Futurice.App.Library.Pages.AddItemPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

addItemPage :: HtmlPage "additempage"
addItemPage = page_ "Add item to Library" (Just NavAddItem) $ do
    div_ $ do
        select_ [data_ "futu-id" "add-new-item-select"] $ do
            optionSelected_ True  [ value_ "Book" ] $ "Add book"
            optionSelected_ False  [ value_ "Boardgame" ] $ "Add boardgame"
        form_ [data_ "futu-id" "add-new-book",recordAction_ addBookPost, method_ "POST", enctype_ "multipart/form-data"] $ do
            table_ $ do
                tr_ $ do
                    th_ "ISBN"
                    td_ $ input_ [ name_ "isbn", type_ "text", required_ ""]
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
                    th_ "Published"
                    td_ $ input_ [ name_ "published", type_ "number", required_ ""]
                tr_ $ do
                    th_ "Amazon link"
                    td_ $ input_ [ name_ "amazon-link", type_ "text"]
                tr_ $ do
                    th_ "Cover"
                    td_ $ input_ [ name_ "cover-file", type_ "file"]
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
                    td_ $ input_ [ name_ "artist", type_ "test"]
                tr_ $ do
                    th_ "Library"
                    td_ $ libraryInput
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
  where
      libraryInput =  ul_ [class_ "no-bullet"] $ for_ usedLibraries $ \lib -> do
          li_ $ do
              input_ [ name_ ("amount-" <> libraryToText lib), type_ "number", min_ "0", style_ "width: 50px; display: inline-block; margin: 0px;"]
              label_ [ style_ "display: inline-block; padding-left: 10px"] (toHtml $ libraryToText lib)
