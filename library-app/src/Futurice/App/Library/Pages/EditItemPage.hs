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

editItemPage :: BookInformation -> HtmlPage "edititempage"
editItemPage info = page_ "Edit book information" (Nothing :: Maybe Nav) $ do
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
