{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic where

import Database.PostgreSQL.Simple.Time (Date)
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Ctx
import Futurice.App.Library.Types

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Personio  as P

textToLibrary :: Text -> Library
textToLibrary "Helsinki" = Helsinki
textToLibrary "Berlin" = Berlin
textToLibrary "Munich" = Munich
textToLibrary "Stockholm" = Stockholm
textToLibrary "Tampere" = Tampere
textToLibrary "Oslo" = Oslo
textToLibrary "London" = London
textToLibrary _ = ELibrary

fetchBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanableId -> m [BookInformationResponse]
fetchBook ctx lid = (fmap . fmap) (\(bookId, title, isbn, author, publisher, published, cover, amazonLink, library)
                                   -> BookInformationResponse bookId title isbn author publisher published cover amazonLink (textToLibrary library))
                 $ safePoolQuery ctx "select library_book.id, title, isbn, author, publisher, published, cover, amazon_link, name from library_book inner join library_library on (library_book.library_id = library_library.id) inner join library_bookinformation on (library_book.information_id = library_bookinformation.id) where library_book.id = ?" $ Only lid

fetchBooks :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformationResponse]
fetchBooks ctx = (fmap . fmap) (\(bookId, title, isbn, author, publisher, published, cover, amazonLink, library)
                                -> BookInformationResponse bookId title isbn author publisher published cover amazonLink (textToLibrary library))
                 $ safePoolQuery ctx "select library_book.id, title, isbn, author, publisher, published, cover, amazon_link, name from library_book inner join library_library on (library_book.library_id = library_library.id) inner join library_bookinformation on (library_book.information_id = library_bookinformation.id)" ()

fetchLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [P.Employee] -> m [Loan]
fetchLoans ctx es = do
    let loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => m [(LoanId, Date, BookId, Integer)]
        loans = safePoolQuery ctx "select id, date_loaned, book_id, person_id from library_loan" ()
        fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => m [BookInformation]
        fetchBookInformation = safePoolQuery ctx "select library_book.id, title, isbn, author, publisher, published, cover, amazon_link, name from library_book inner join library_library on (library_book.library_id = library_library.id) inner join library_bookinformation on (library_book.information_id = library_bookinformation.id)" ()
        bookMap = (Map.fromList . fmap (\x -> (_bookId x, x)))
        bookData bid bm = bm ^.at bid
        bookInformationToLoanable book = Loanable (Book book) (_bookLibrary book)
        employeeMap = Map.fromList . map (\e -> (e ^. P.employeeId, e))
        idToName emap pid = case emap ^.at pid of
          Just employee -> (employee ^. P.employeeFirst) <> (employee ^. P.employeeLast)
          Nothing -> "Unknown"
        loans' :: [(LoanId, Date, BookId, Integer)] -> Map P.EmployeeId P.Employee -> Map BookId BookInformation -> [Loan]
        loans' ls emap bmap = concat $ fmap (\(loanId, date, bookId, personio_id) ->
            case bookData bookId bmap of
              Just bookInfo -> [Loan loanId (T.pack $ show date) (bookInformationToLoanable bookInfo) (idToName emap $ P.EmployeeId (fromIntegral personio_id))]
              Nothing -> [] ) ls
    l <- loans
    fs <- fetchBookInformation
    pure $ loans' l (employeeMap es) $ bookMap fs
