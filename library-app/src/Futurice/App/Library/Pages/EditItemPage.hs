{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.EditItemPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Data.Text as T

editItemPage :: Either (BookInformation, [(ItemId, LoanStatus, Library)]) (BoardGameInformation, [(ItemId, LoanStatus, Library)]) -> HtmlPage "edititempage"
editItemPage (Left (info, books)) = page_ "Edit book information" (Nothing :: Maybe Nav) $ do
    form_ [data_ "futu-id" "edit-book", data_ "form-type" "book", recordAction_ editBookPost, method_ "POST", enctype_ "multipart/form-data"] $ do
        input_ [ name_ "bookinformationid", type_ "hidden", value_ ((T.pack . show) $ info ^. bookInformationId)]
        table_ $ do
            tr_ $ do
                th_ "ISBN"
                td_ $ input_ [ name_ "isbn", id_ "isbn", type_ "text", required_ "", value_ (info ^. bookISBN)]
            tr_ $ do
                th_ "Title"
                td_ $ input_ [ name_ "title", type_ "text", required_ "", value_ (info ^. bookTitle)]
            tr_ $ do
                th_ "Author"
                td_ $ input_ [ name_ "author", type_ "text", required_ "", value_ (info ^. bookAuthor)]
            tr_ $ do
                th_ "Publisher"
                td_ $ input_ [ name_ "publisher", type_ "text", required_ "", value_ (info ^. bookPublisher)]
            tr_ $ do
                th_ "Published"
                td_ $ input_ [ name_ "published", type_ "number", required_ "", value_ ((T.pack . show) $ info ^. bookPublished)]
            tr_ $ do
                th_ "Amazon link"
                td_ $ input_ [ name_ "amazon-link", type_ "text", value_ (info ^. bookAmazonLink)]
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Edit"
    h3_ "Books"
    ul_ [class_ ""] $ do
        for_ (sortBy (\(_,_,lib1) (_,_,lib2) -> librarySelectSortOrder lib1 lib2) books) $ \(itemid, status, library) ->
          li_ $ do
            span_ [style_ "padding-right: 10px;"] $ toHtml $ library
            case status of
              Loaned -> "Loaned"
              NotLoaned -> button_ [ class_ "tiny button success", data_ "futu-id" "delete-item", data_ "item-id" (T.pack $ show itemid)] $ "Delete"
        li_ $ do
            form_ [data_ "futu-id" "add-new-book-item", recordAction_ addItemPost, method_ "POST", enctype_ "multipart/form-data"] $ do
                input_ [ name_ "bookinformationid", type_ "hidden", value_ (T.pack $ show $ info ^. bookInformationId)]
                select_ [name_ "library", style_ "width: 10%;"] $ do
                    for_ usedLibraries $ \lib ->
                      optionSelected_ False [value_ (libraryToText lib)] $ toHtml lib
                button_ [ class_ "button success tiny", data_ "futu-action" "submit", data_ "rel" "back" ] $ "Add new book"


editItemPage (Right (info, _)) =  page_ "Edit board game information" (Nothing :: Maybe Nav) $ do
    form_ [data_ "futu-id" "edit-boardgame", data_ "form-type" "boardgame", recordAction_ editBoardGamePost, method_ "POST", enctype_ "multipart/form-data"] $ do
        input_ [ name_ "boardgameinformationid", type_ "hidden", value_ ((T.pack . show) $ info ^. boardGameInformationId)]
        table_ $ do
            tr_ $ do
                th_ "Name"
                td_ $ input_ [ name_ "name", type_ "text", required_ "", value_ (info ^. boardGameName)]
            tr_ $ do
                th_ "Publisher"
                td_ $ input_ [ name_ "publisher", type_ "text", value_ (fromMaybe "" $ info ^. boardGamePublisher)]
            tr_ $ do
                th_ "Published"
                td_ $ input_ [ name_ "published", type_ "number", value_ (maybe "" (T.pack . show) $ info ^. boardGamePublished)]
            tr_ $ do
                th_ "Designer"
                td_ $ input_ [ name_ "designer", type_ "text", value_ (fromMaybe "" $ info ^. boardGameDesigner)]
            tr_ $ do
                th_ "Artist"
                td_ $ input_ [ name_ "artist", type_ "text", value_ (fromMaybe "" $ info ^. boardGameArtist)]
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Edit"
