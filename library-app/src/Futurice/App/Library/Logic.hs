{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic where

import Data.List
import Database.PostgreSQL.Simple         (In (..))
import Database.PostgreSQL.Simple.FromRow
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

data BookData = BookData BookId BookInformationId Library deriving (Generic, FromRow)
data LoanData = LoanData LoanId Day BookId Integer deriving (Generic, FromRow)

idToName :: IdMap P.Employee -> P.EmployeeId -> Text
idToName emap pid = case emap ^.at pid of
  Just employee -> (employee ^. P.employeeFirst) <> " " <> (employee ^. P.employeeLast)
  Nothing -> "Unknown"

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

fetchLoanByBookId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookId -> m (Maybe LoanData)
fetchLoanByBookId ctx bid = listToMaybe <$> safePoolQuery ctx "select id, date_loaned, book_id, personio_id from library_loan where book_id = ?" (Only bid)

makeLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Day -> BookId -> P.EmployeeId -> m (Maybe LoanData)
makeLoan ctx date bid (P.EmployeeId eid) = do
    let tempNumber = 9999 :: Integer
    _ <- logInfo_ "Inserting to library_loan"
    _ <- safePoolExecute ctx "INSERT INTO library_loan (date_loaned, book_id, person_id, personio_id) VALUES (?,?,?,?)" (date, bid, tempNumber, toInteger eid)
    listToMaybe <$> safePoolQuery ctx "SELECT id, date_loaned, book_id, personio_id FROM library_loan where book_id = ?" (Only bid)

fetchBooksByBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [BookData]
fetchBooksByBookInformation ctx binfoid = safePoolQuery ctx "select library_book.id, information_id, name from library_book inner join library_library on library_book.library_id = library_library.id where library_book.information_id = ?" (Only binfoid)

fetchBookWithoutLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [BookData] -> m (Maybe BookId)
fetchBookWithoutLoans ctx books = do
    _ <- logInfo_ "Fetching free books"
    ls <- safePoolQuery ctx "SELECT id, date_loaned, book_id, personio_id FROM library_loan where book_id in ?;" (Only (In bookids))
    pure $ listToMaybe $ bookids \\ ((\(LoanData _ _ bid _) -> bid) <$> ls)
  where
      bookids = (\(BookData bid _ _ ) -> bid) <$> books

borrowBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> P.EmployeeId -> BorrowRequest -> m (Maybe LoanData)
borrowBook ctx eid (BorrowRequest binfoid _) = do
    now <- currentDay
    _ <- logInfo_ "Fetching books"
    books <- fetchBooksByBookInformation ctx binfoid
    runMaybeT $ do
        freeBook <- MaybeT $ fetchBookWithoutLoans ctx books
        MaybeT $ makeLoan ctx now freeBook eid

fetchBooksInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformation]
fetchBooksInformation ctx = safePoolQuery ctx "SELECT id, title, isbn, author, publisher, published, cover, amazon_link FROM library_bookinformation" ()

fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m (Maybe BookInformation)
fetchBookInformation ctx bid = fmap listToMaybe $ safePoolQuery ctx "SELECT id, title, isbn, author, publisher, published, cover, amazon_link FROM library_bookinformation WHERE id = ?" (Only bid)

fetchBookResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [BookInformationResponse]
fetchBookResponse ctx binfoid = do
    books <- fetchBooksByBookInformation ctx binfoid
    binfo <- safePoolQuery ctx "SELECT id, title, isbn, author, publisher, published, cover, amazon_link FROM library_bookinformation WHERE id = ?" $ Only binfoid
    case listToMaybe binfo of
      Nothing -> pure $ []
      Just info -> pure $ [toBookInformationResponse (fold (toBooks <$> books)) info]
  where
      toBooks (BookData bid _ lib) = [Books bid lib]
      toBookInformationResponse books (infoid, title, isbn, author, publisher, published, cover, amazonLink) =
          BookInformationResponse infoid title isbn author publisher published cover amazonLink books

fetchBooksResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformationResponse]
fetchBooksResponse ctx = do
    books <- fetchBooks ctx
    bookInfos <- safePoolQuery ctx "SELECT id, title, isbn, author, publisher, published, cover, amazon_link FROM library_bookinformation" ()
    pure $ fmap (toBookInformationResponse (Map.fromListWith (++) (toBooks <$> books) )) bookInfos
  where
      toBooks (BookData bid binfoid lib) = (binfoid, [Books bid lib])
      toBookInformationResponse books (binfoid, title, isbn, author, publisher, published, cover, amazonLink) =
          BookInformationResponse binfoid title isbn author publisher published cover amazonLink (fromMaybe [] (books ^.at binfoid))

fetchBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookId -> m (Maybe BookData)
fetchBook ctx bid = listToMaybe <$> ((fmap . fmap) (\(bookid, binfo, lib) -> BookData bookid binfo lib) $ (safePoolQuery ctx "SELECT library_book.id, information_id, name FROM library_book INNER JOIN library_library ON library_book.library_id = library_library.id WHERE library_book.id = ?" $ Only bid))

fetchBooks :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookData]
fetchBooks ctx = (fmap . fmap) (\(bid, binfo, lib) -> BookData bid binfo lib) $ safePoolQuery ctx "select library_book.id, information_id, name from library_book inner join library_library on library_book.library_id = library_library.id" ()

loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [LoanData]
loans ctx = safePoolQuery ctx "select id, date_loaned, book_id, personio_id from library_loan" ()

loan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m (Maybe LoanData)
loan ctx lid = fmap listToMaybe $ safePoolQuery ctx "select id, date_loaned, book_id, personio_id from library_loan where id = ?" (Only lid)

bookInformationToLoanable :: BookId -> BookInformation -> Library -> Loanable
bookInformationToLoanable bid book = Loanable (Book bid book)

fetchLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> IdMap P.Employee -> m (Maybe Loan)
fetchLoan ctx lid es = do
    runMaybeT $ do
        l <- MaybeT $ loan ctx lid
        b <- MaybeT $ fetchBook ctx $ bookid l
        binfo <- MaybeT $ fetchBookInformation ctx $ (\(BookData _ binfoid _) -> binfoid) b
        pure $ loan' l es binfo $ bookLibrary b
  where
      bookid (LoanData _ _ bid _) = bid
      bookLibrary (BookData _ _ blib) = blib
      loan' (LoanData loanId date bookId personio_id) emap bookInfo lib = Loan loanId (T.pack $ show date) (Loanable (Book bookId bookInfo) lib) (idToName emap $ P.EmployeeId (fromIntegral personio_id))

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
      bookMap = Map.fromList . fmap (\x -> (_bookInformationId x, x))
      bookLibrary bid bmap =
          case bmap ^. at bid of
            Just (_, lib) -> lib
            Nothing -> UnknownLibrary
      bookIdToLoanable bs bmap bid = case bookIdToInformation bid bs bmap of
        Just info -> (bookInformationToLoanable bid info $ bookLibrary bid bs)
        Nothing -> Loanable (NotFound "Couldn't find information for book") UnknownLibrary
      loans' ::  [LoanData] -> IdMap P.Employee -> Map BookId (BookInformationId, Library) -> Map BookInformationId BookInformation -> [Loan]
      loans' ls emap bs bmap = fmap (\(LoanData loanId date bid personio_id) ->
        Loan loanId (T.pack $ show date) (bookIdToLoanable bs bmap bid) (idToName emap $ P.EmployeeId (fromIntegral personio_id))) ls
