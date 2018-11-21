{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.PersonalLoansPage (personalLoansPage) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Data.Text as T

personalLoansPage :: [Loan] -> HtmlPage "personalinformation"
personalLoansPage loans = page_ "My loans" (Just NavUser) $ do
    let ItemBuckets bookLoans boardgameLoans = loanSorter loans
    if null bookLoans && null boardgameLoans then
        "No loans"
    else do
        unless (null bookLoans) $ do
            h2_ $ "Books"
            table_ $ do
                thead_ $ tr_ $ do
                    th_ "Title"
                    th_ "Author"
                    th_ "Publisher"
                    th_ "Published"
                    th_ "ISBN"
                    th_ ""
                tbody_ $ for_ bookLoans $ \(WithLoanId lid book') -> tr_ $ do
                    let book = fromItemBook book'
                    td_ $ toHtml $ book ^. bookTitle
                    td_ $ toHtml $ book ^. bookAuthor
                    td_ $ toHtml $ book ^. bookPublisher
                    td_ $ toHtml $ show $ book ^. bookPublished
                    td_ $ toHtml $ book ^. bookISBN
                    td_ $ button_ [class_ "button", data_ "futu-id" "return-loan", data_ "loan-id" (T.pack $ show lid)] $ toHtml ("Return" :: Text)
        unless (null boardgameLoans) $ do
            h2_ $ "Boardgames"
            table_ $ do
                thead_ $ tr_ $ do
                    th_ "Name"
                    th_ "Publisher"
                    th_ "Published"
                    th_ "Designer"
                    th_ "Artist"
                    th_ ""
                tbody_ $ for_ boardgameLoans $ \(WithLoanId lid boardgame') -> tr_ $ do
                    let boardgame = fromItemBoardGame boardgame'
                    td_ $ toHtml $ boardgame ^. boardGameName
                    td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGamePublisher
                    td_ $ toHtml $ show $ boardgame ^. boardGamePublished
                    td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGameDesigner
                    td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGameArtist
                    td_ $ button_ [class_ "button", data_ "futu-id" "return-loan", data_ "loan-id" (T.pack $ show lid)] $ toHtml ("Return" :: Text)
  where
    loanSorter :: [Loan] -> ItemBuckets (WithLoanId ItemInfo)
    loanSorter = partitionItems $ \loan ->
        case loan ^. loanInformation . itemInfo of
            MkSome ii -> MkSome $ WithLoanId (loan ^. loanId) ii

data WithLoanId f (ty :: ItemType) = WithLoanId LoanId (f ty)

instance HasItemType f => HasItemType (WithLoanId f) where
    itemType (WithLoanId _ i) = itemType i
