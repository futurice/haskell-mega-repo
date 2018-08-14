{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.PersonalLoansPage where

import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

personalLoansPage :: [Loan] -> HtmlPage ("personalinformation")
personalLoansPage loans = page_ "My loans" (Just NavUser) $ do
    fullRow_ $ do
        for_ (listToMaybe $ fst $ loanSorter loans) $ \_ -> do
            h1_ $ "Books"
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
                    for_ (fst $ loanSorter loans) $ \(lid, book) -> do
                        tr_ $ do
                            td_ $ toHtml $ book ^. bookTitle
                            td_ $ toHtml $ book ^. bookAuthor
                            td_ $ toHtml $ book ^. bookPublisher
                            td_ $ toHtml $ show $ book ^. bookPublished
                            td_ $ toHtml $ book ^. bookISBN
                            td_ $ a_ [href_ $ linkToText $ fieldLink returnPost lid] $ toHtml ("Return" :: Text)
        for_ (listToMaybe $ snd $ loanSorter loans) $ \_ -> do
            h1_ $ "Boardgames"


  where
    loanSorter :: [Loan] -> ([(LoanId, BookInformation)], [(LoanId, BoardGameInformation)])
    loanSorter [] = ([],[])
    loanSorter (l : ls) = case l ^. loanInformation ^. itemInfo of
        ItemBook book -> let nextValue = loanSorter ls
                         in ([(l ^. loanId, book)] ++ (fst nextValue), snd nextValue)
        ItemBoardGame boardgame -> let nextValue = loanSorter ls
                                   in (fst nextValue, [(l ^. loanId, boardgame)] ++ (snd nextValue))
