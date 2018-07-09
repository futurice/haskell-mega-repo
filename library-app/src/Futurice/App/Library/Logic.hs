{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic where

import Database.PostgreSQL.Simple.Time (Date)
import FUM.Types.Login
import Futurice.IdMap
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

migrateToPersonioNumber :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Map Login P.Employee -> m ()
migrateToPersonioNumber ctx emap = do
    let getAuthUsers :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => m [(Integer, Text)]
        getAuthUsers = safePoolQuery ctx "select id, username from auth_user" ()
        textToLoginToEmployeeid login = case parseLogin' login of
                                          Right l -> (emap ^.at l) >>= (\x -> Just $ (x ^. P.employeeId))
                                          Left _ -> Nothing
        userNameToPersonio :: (Integer, Text) -> (Integer, Maybe P.EmployeeId)
        userNameToPersonio (authId, eid) = (authId, textToLoginToEmployeeid eid)
        updateLoan :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => (Integer, Maybe P.EmployeeId) -> m Int64
        updateLoan (lid, Just (P.EmployeeId eid)) = safePoolExecute ctx "UPDATE library_loan SET personio_id = ? where person_id = ?" (eid, lid)
        updateLoan (_, Nothing) = pure 0
        --loanToloan (loanid,_,_,authid) = (,loanid)
    authUsers <- getAuthUsers
    mapM_ updateLoan $ map userNameToPersonio authUsers

loanBook :: Ctx -> BookId -> P.Employee -> Maybe Loan
loanBook = undefined

fetchBooksInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformation]
fetchBooksInformation ctx = safePoolQuery ctx "select id, title, isbn, author, publisher, published, cover, amazon_link from library_bookinformation" ()

fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m (Maybe BookInformation)
fetchBookInformation ctx bid = fmap listToMaybe $ safePoolQuery ctx "select id, title, isbn, author, publisher, published, cover, amazon_link from library_bookinformation id = ?" (Only bid)

fetchBookResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookId -> m [BookInformationResponse]
fetchBookResponse ctx lid = (fmap . fmap) (\(bookId, title, isbn, author, publisher, published, cover, amazonLink, library)
                                   -> BookInformationResponse bookId title isbn author publisher published cover amazonLink (textToLibrary library))
                 $ safePoolQuery ctx "select library_book.id, title, isbn, author, publisher, published, cover, amazon_link, name from library_book inner join library_library on (library_book.library_id = library_library.id) inner join library_bookinformation on (library_book.information_id = library_bookinformation.id) where library_book.id = ?" $ Only lid

fetchBooksResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformationResponse]
fetchBooksResponse ctx = (fmap . fmap) (\(bookId, title, isbn, author, publisher, published, cover, amazonLink, library)
                                -> BookInformationResponse bookId title isbn author publisher published cover amazonLink (textToLibrary library))
                 $ safePoolQuery ctx "select library_book.id, title, isbn, author, publisher, published, cover, amazon_link, name from library_book inner join library_library on (library_book.library_id = library_library.id) inner join library_bookinformation on (library_book.information_id = library_bookinformation.id)" ()

data BookData = BookData BookId BookInformationId Library

fetchBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookId -> m (Maybe BookData)
fetchBook ctx bid = listToMaybe <$> ((fmap . fmap) (\(bid, binfo, lib) -> BookData bid binfo lib) $ (safePoolQuery ctx "select id, information_id, name inner join library_library on library_book.library_id = library_library.id where library_book.id = ?" $ Only bid))

fetchBooks :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookData]
fetchBooks ctx = (fmap . fmap) (\(bid, binfo, lib) -> BookData bid binfo lib) $ safePoolQuery ctx "select id, information_id, name inner join library_library on library_book.library_id = library_library.id from library_book" ()

loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [(LoanId, Date, BookId, Integer)]
loans ctx = safePoolQuery ctx "select id, date_loaned, book_id, personio_id from library_loan" ()

loan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m (Maybe (LoanId, Date, BookId, Integer))
loan ctx lid = fmap listToMaybe $ safePoolQuery ctx "select id, date_loaned, book_id, personio_id from library_loan where id = ?" (Only lid)

bookInformationToLoanable :: BookId -> BookInformation -> Library -> Loanable
bookInformationToLoanable bid book lib = Loanable (Book bid book) lib

fetchLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> IdMap P.Employee -> m (Maybe Loan)
fetchLoan ctx lid es = do
    runMaybeT $ do
        l <- MaybeT $ loan ctx lid
        b <- MaybeT $ fetchBook ctx $ bookid l
        binfo <- MaybeT $ fetchBookInformation ctx $ (\(BookData _ binfoid _) -> binfoid) b
        pure $ loan' l es binfo $ bookLibrary b
  where
      bookid (_,_,bid,_) = bid
      bookLibrary (BookData _ _ blib) = blib
      idToName emap pid = case emap ^.at pid of
        Just employee -> (employee ^. P.employeeFirst) <> (employee ^. P.employeeLast)
        Nothing -> "Unknown"
      loan' (loanId, date, bookId, personio_id) emap bookInfo lib = Loan loanId (T.pack $ show date) (Loanable (Book bookId bookInfo) lib) (idToName emap $ P.EmployeeId (fromIntegral personio_id))

bookArrayToMap :: [BookData] -> Map BookId (BookInformationId, Library)
bookArrayToMap = Map.fromList . fmap (\(BookData bid binfoid lib) -> (bid, (binfoid, lib)))

bookIdToInformation :: BookId -> Map BookId (BookInformationId, Library) -> Map BookInformationId BookInformation -> Maybe BookInformation
bookIdToInformation bid bookidmap bookinfomap = bookidmap ^.at bid >>= (\(infoid, _) -> bookinfomap ^.at infoid)

fetchLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> m [Loan]
fetchLoans ctx es = do
    l <- loans ctx
    bs <- fetchBooks ctx
    fs <- fetchBooksInformation ctx
    pure $ loans' l es (bookArrayToMap bs) $ bookMap fs
  where
      bookMap = (Map.fromList . fmap (\x -> (_bookInformationId x, x)))
      bookId (bid,_,_) = bid
      bookLibrary bid bmap =
          case bmap ^. at bid of
            Just (binfo, lib) -> lib
            Nothing -> Unknown
      idToName emap pid = case emap ^.at pid of
        Just employee -> (employee ^. P.employeeFirst) <> " " <> (employee ^. P.employeeLast)
        Nothing -> "Unknown"
      bookIdToLoanable bs bmap bid = case bookIdToInformation bid bs bmap of
        Just info -> (bookInformationToLoanable bid info $ bookLibrary bid bs)
        Nothing -> Loanable (NotFound "Couldn't find information for book") Unknown
      loans' ::  [(LoanId, Date, BookId, Integer)] -> IdMap P.Employee -> Map BookId (BookInformationId, Library) -> Map BookInformationId BookInformation -> [Loan]
      loans' ls emap bs bmap = fmap (\(loanId, date, bookId, personio_id) ->
        Loan loanId (T.pack $ show date) (bookIdToLoanable bs bmap bookId) (idToName emap $ P.EmployeeId (fromIntegral personio_id))) ls
