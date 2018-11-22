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
        div_ [ class_ "large-2 columns"] $ do
            img_ [style_ "max-height: 400px; display: block; margin-left: auto; margin-right: auto;", src_ $ linkToText $ fieldLink bookCoverGet cover ]
        div_ [ class_ "large-10 columns"] $ do
            table_ $ do
                vertRow_ "Title" $ toHtml $ title
                vertRow_ "Author" $ toHtml $ author
                vertRow_ "Publisher" $ toHtml $ publisher
                vertRow_ "Published" $ toHtml $ show published
                vertRow_ "ISBN" $ toHtml $ isbn
            a_ [class_ "button small", href_ $ linkToText $ fieldLink editBookPageGet binfoid] "Edit book information"
    fullRow_ $ do
        h2_ "Books"
        for_ officeMap $ \(lib, bs) -> do
            case lib of
              OfficeLibrary library -> do
                  h3_ $ T.pack $ show $ toHtml library
                  unless (null $ (snd . partitionByLoan) bs) $ do
                      span_ [style_ "padding-left: 10px; padding-right: 10px;"] $ toHtml $ (availabilityText . snd . partitionByLoan) bs
                      button_ [class_ "button tiny",
                               data_ "futu-id" "loan-item",
                               data_ "item-id" (T.pack $ show binfoid),
                               data_ "library" (T.pack $ show $ toHtml library)] $ toHtml ("Borrow" :: Text)
                  unless (null $ fst (partitionByLoan bs)) $ table_ $ do
                      thead_ $ tr_ $ th_ $ toHtml $ idT $ "Copies on loan "
                      tbody_ $ for_ (fst $ partitionByLoan bs) $ \b ->
                        for_ (loanMap ^.at (_booksBookId b)) $ \(_, day, person) -> tr_ $ do
                          td_ $ toHtml $ (idToName es $ person)
                          td_ $ do
                              div_ [ style_ "float: left;"] $ toHtml $ T.pack $ show day
                              button_ [class_ "button tiny",
                                          data_ "futu-id" "snatch-item",
                                          data_ "item-id" (T.pack $ show (_booksBookId b))] $ toHtml ("Take over" :: Text)
              _ -> pure ()
    where
      idT :: Text -> Text
      idT = id
      officeMap = M.toList $ M.fromListWith (++) $ (\x -> (_booksLibrary x, [x])) <$> books
      loanMap = M.fromList $ (\(LoanData lid day person iid) -> (iid, (lid, day, person))) <$> ls
      partitionByLoan = partition (\x -> isJust (loanMap ^.at (_booksBookId x)))
      availabilityText bs | length bs > 1 = (show . length) bs <> " books available"
                          | otherwise     = "1 book available"
