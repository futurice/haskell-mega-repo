{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.PersonalLoansPage where

import Data.Either      (partitionEithers)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Data.Text as T

personalLoansPage :: [Loan] -> HtmlPage ("personalinformation")
personalLoansPage loans = page_ "My loans" (Just NavUser) $ do
    let (bookLoans, boardgameLoans) = loanSorter loans
    unless (null bookLoans && null boardgameLoans) $ fullRow_ $ do
        unless (null bookLoans) $ do
            h2_ $ "Books"
            table_ $ do
                thead_ $ do
                    tr_ $ do
                        th_ "Title"
                        th_ "Author"
                        th_ "Publisher"
                        th_ "Published"
                        th_ "ISBN"
                        th_ ""
                tbody_ $ do
                    for_ bookLoans $ \(lid, book) -> do
                        tr_ $ do
                            td_ $ toHtml $ book ^. bookTitle
                            td_ $ toHtml $ book ^. bookAuthor
                            td_ $ toHtml $ book ^. bookPublisher
                            td_ $ toHtml $ show $ book ^. bookPublished
                            td_ $ toHtml $ book ^. bookISBN
                            td_ $ button_ [class_ "button", data_ "futu-id" "return-loan", data_ "loan-id" (T.pack $ show lid)] $ toHtml ("Return" :: Text)
        unless (null boardgameLoans) $ do
            h2_ $ "Boardgames"
            table_ $ do
                thead_ $ do
                    tr_ $ do
                        th_ "Name"
                        th_ "Publisher"
                        th_ "Published"
                        th_ "Designer"
                        th_ "Artist"
                        th_ ""
                tbody_ $ do
                    for_ boardgameLoans $ \(lid, boardgame) -> do
                        tr_ $ do
                            td_ $ toHtml $ boardgame ^. boardGameName
                            td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGamePublisher
                            td_ $ toHtml $ show $ boardgame ^. boardGamePublished
                            td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGameDesigner
                            td_ $ toHtml $ fromMaybe "" $ boardgame ^. boardGameArtist
                            td_ $ button_ [class_ "button", data_ "futu-id" "return-loan", data_ "loan-id" (T.pack $ show lid)] $ toHtml ("Return" :: Text)
  where
    loanSorter :: [Loan] -> ([(LoanId, BookInformation)], [(LoanId, BoardGameInformation)])
    loanSorter ls = let sortLoan (lid, ItemBook book) = Left (lid, book)
                        sortLoan (lid, ItemBoardGame boardgame) = Right (lid, boardgame)
                    in partitionEithers $ sortLoan <$> (\l -> (l ^. loanId, l ^. loanInformation ^. itemInfo)) <$> ls
