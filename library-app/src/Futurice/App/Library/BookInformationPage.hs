{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.BookInformationPage where

import Data.List                 (partition)
import Data.Maybe                (isJust)
import Futurice.IdMap            (IdMap)
import Futurice.Lucid.Foundation
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
bookInformationPage (BookInformationResponse _binfoid title isbn author publisher published cover _amazonLink books) ls es = page_ "Book details " pageParams $ do
    navigation_ Nothing
    fullRow_ $ do
        div_ [] $ do
            img_ [src_ $ linkToText $ fieldLink bookCoverGet cover ]
        div_ [] $ do
            table_ $ do
                tr_ $ do
                    th_ "Title"
                    td_ $ toHtml $ title
                tr_ $ do
                    th_ "Author"
                    td_ $ toHtml $ author
                tr_ $ do
                    th_ "Publisher"
                    td_ $ toHtml $ show publisher
                tr_ $ do
                    th_ "Published"
                    td_ $ toHtml $ show published
                tr_ $ do
                    th_ "ISBN"
                    td_ $ toHtml $ isbn
    fullRow_ $ do
        title_ "Books"
        for_ officeMap $ \(lib, bs) -> do
            case lib of
              OfficeLibrary library -> do
                  row_ $ toHtml library
                  row_ $ for_ (listToMaybe $ (snd . partitionByLoan) bs) $ \_ -> do
                      toHtml $ (show . length . snd . partitionByLoan) bs <> " books available"
                  row_ $ do
                      span_ $ toHtml $ idT $ "Copies on loan "
                      for_ (fst $ partitionByLoan bs) $ \b ->
                        for_ (loanMap ^.at (_booksBookId b)) $ \(_, day, person) -> span_ $ toHtml $ (idToName es $ (P.EmployeeId . fromIntegral) person) <> " " <> (T.pack $ show day)
              _ -> pure ()
    where
      idT :: Text -> Text
      idT = id
      officeMap = M.toList $ M.fromListWith (++) $ (\x -> (_booksLibrary x, [x])) <$> books
      loanMap = M.fromList $ (\(LoanData lid day bid person) -> (bid, (lid, day, person))) <$> ls
      partitionByLoan = partition (\x -> isJust (loanMap ^.at (_booksBookId x)))
