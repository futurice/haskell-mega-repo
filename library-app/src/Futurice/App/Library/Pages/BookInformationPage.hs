{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Pages.BookInformationPage where

import Data.List        (partition)
import Data.Maybe       (isJust)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Logic  (LoanData (..), idToName)
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Personio  as P

bookInformationPage :: BookInformationResponse -> [LoanData] -> IdMap P.Employee -> HtmlPage ("bookinformation")
bookInformationPage (BookInformationResponse binfoid title isbn author publisher published cover _amazonLink books) ls es = page_ "Book details " (Nothing :: Maybe Nav) $ do
    fullRow_ $ do
        div_ [] $ do
            img_ [src_ $ linkToText $ fieldLink bookCoverGet cover ]
        div_ [] $ do
            a_ [class_ "button small", href_ $ linkToText $ fieldLink editBookPageGet binfoid] "Edit book information"
            table_ $ do
                tr_ $ do
                    th_ "Title"
                    td_ $ toHtml $ title
                tr_ $ do
                    th_ "Author"
                    td_ $ toHtml $ author
                tr_ $ do
                    th_ "Publisher"
                    td_ $ toHtml $ publisher
                tr_ $ do
                    th_ "Published"
                    td_ $ toHtml $ show published
                tr_ $ do
                    th_ "ISBN"
                    td_ $ toHtml $ isbn
    fullRow_ $ do
        h2_ "Books"
        for_ officeMap $ \(lib, bs) -> do
            case lib of
              OfficeLibrary library -> do
                  h3_ $ T.pack $ show $ toHtml library
                  for_ (listToMaybe $ (snd . partitionByLoan) bs) $ \_ -> do
                      span_ [style_ "padding-left: 10px; padding-right: 10px;"] $ toHtml $ (show . length . snd . partitionByLoan) bs <> " books available"
                      button_ [class_ "button tiny",
                               data_ "futu-id" "loan-item",
                               data_ "item-id" (T.pack $ show binfoid),
                               data_ "library" (T.pack $ show $ toHtml library)] $ toHtml ("Borrow" :: Text)
                  for_ (listToMaybe $ fst (partitionByLoan bs)) $ \_ -> table_ $ do
                      thead_ $ tr_ $ th_ $ toHtml $ idT $ "Copies on loan "
                      tbody_ $ for_ (fst $ partitionByLoan bs) $ \b ->
                        for_ (loanMap ^.at (_booksBookId b)) $ \(_, day, person) -> tr_ $ do
                          td_ $ toHtml $ (idToName es $ person)
                          td_ $ toHtml $ T.pack $ show day
              _ -> pure ()
    where
      idT :: Text -> Text
      idT = id
      officeMap = M.toList $ M.fromListWith (++) $ (\x -> (_booksLibrary x, [x])) <$> books
      loanMap = M.fromList $ (\(LoanData lid day person iid) -> (iid, (lid, day, person))) <$> ls
      partitionByLoan = partition (\x -> isJust (loanMap ^.at (_booksBookId x)))
