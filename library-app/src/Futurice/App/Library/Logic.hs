{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic where

import Control.Monad                      (replicateM_)
import Data.List
import Database.PostgreSQL.Simple         (In (..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Futurice.App.Sisosota.Types        (ContentHash)
import Futurice.IdMap
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Personio  as P

-------------------------------------------------------------------------------
-- Helper datatypes
-------------------------------------------------------------------------------

data InfoId = BookInfoId BookInformationId
            | BoardGameInfoId BoardGameInformationId
            deriving (Show)

data ItemData = ItemData
    { idItemId  :: !ItemId
    , idLibrary :: !Library
    , idInfoId  :: !InfoId
    } deriving (Show)

data LoanData = LoanData
    { ldLoanId        :: !LoanId
    , ldDateLoaned    :: !Day
    , ldPersonioId    :: !(P.EmployeeId)
    , ldItemId        :: !ItemId
    } deriving (Show)

instance FromRow LoanData where
    fromRow = LoanData
        <$> field
        <*> field
        <*> (P.EmployeeId . (fromIntegral :: Int32 -> Word) <$> field)
        <*> field

instance FromRow ItemData where
    fromRow = do
        itemId        <- field
        library       <- field
        bookInfo      <- field
        boardGameInfo <- field
        case BookInfoId <$> bookInfo <|> BoardGameInfoId <$> boardGameInfo of
            Just infoid -> pure $ ItemData itemId library infoid
            Nothing     -> empty

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

idToName :: IdMap P.Employee -> P.EmployeeId -> Text
idToName emap pid = case emap ^.at pid of
  Just employee -> (employee ^. P.employeeFirst) <> " " <> (employee ^. P.employeeLast)
  Nothing -> "Unknown"

itemArrayToMap :: [ItemData] -> Map ItemId (InfoId, Library)
itemArrayToMap = Map.fromList . fmap (\(ItemData iid lib infoid) -> (iid, (infoid, lib)))

-------------------------------------------------------------------------------
-- Item functions
-------------------------------------------------------------------------------

fetchItemsWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> [ItemId] -> m [ItemData]
fetchItemsWithItemIds ctx iids = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id in ?;" (Only (In iids))

fetchItemsWithoutLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> [ItemData] -> m (Maybe ItemId)
fetchItemsWithoutLoans ctx items = do
    ls <- fetchLoansWithItemIds ctx iids
    pure $ listToMaybe $ iids \\ (ldItemId <$> ls)
  where
    iids = idItemId <$> items

fetchItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> ItemId -> m (Maybe ItemData)
fetchItem ctx iid = listToMaybe <$> (safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id = ?" $ Only iid)

fetchItems :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [ItemData]
fetchItems ctx = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item " ()

deleteItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> ItemId -> m Bool
deleteItem ctx iid = (1 ==) <$> safePoolExecute ctx "DELETE FROM library.item WHERE item_id = ?" (Only iid)

addItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> AddItemRequest -> m Bool
addItem ctx (AddItemRequest lib bookinfo_id boardgameinfo_id) =
    (1 ==) <$> safePoolExecute ctx "INSERT INTO library.item (library, bookinfo_id, boardgameinfo_id) VALUES (?,?,?)" (lib, bookinfo_id, boardgameinfo_id)

fetchItemsByBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BookInformationId -> m [ItemData]
fetchItemsByBookInformation ctx binfoid = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE bookinfo_id = ?" (Only binfoid)

fetchItemsByBoardGameInformationId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BoardGameInformationId -> m [ItemData]
fetchItemsByBoardGameInformationId ctx binfoid = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE boardgameinfo_id = ?" (Only binfoid)

-------------------------------------------------------------------------------
-- Loan functions
-------------------------------------------------------------------------------

fetchLoanByItemId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> ItemId -> m (Maybe LoanData)
fetchLoanByItemId ctx iid = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id = ?" (Only iid)

makeLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> Day -> ItemId -> P.EmployeeId -> m (Maybe LoanData)
makeLoan ctx date iid (P.EmployeeId eid) = do
    _ <- safePoolExecute ctx "INSERT INTO library.loan (date_loaned, personio_id, item_id) VALUES (?,?,?)" (date, toInteger eid, iid)
    listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id = ?" (Only iid)

fetchLoansWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> [ItemId] -> m [LoanData]
fetchLoansWithItemIds ctx iids = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id in ?;" (Only (In iids))

borrowBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> P.EmployeeId -> BorrowRequest -> m (Maybe LoanData)
borrowBook ctx eid (BorrowRequest binfoid _) = do
    now <- currentDay
    books <- fetchItemsByBookInformation ctx binfoid
    runMaybeT $ do
        freeBook <- MaybeT $ fetchItemsWithoutLoans ctx books
        MaybeT $ makeLoan ctx now freeBook eid

loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [LoanData]
loans ctx = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan" ()

loan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> LoanId -> m (Maybe LoanData)
loan ctx lid = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where loan_id = ?" (Only lid)

fetchLoanIdWithItemId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> ItemId -> m (Maybe LoanData)
fetchLoanIdWithItemId ctx itemId = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id = ?" (Only itemId)

checkLoanStatus :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> ItemId -> m LoanStatus
checkLoanStatus ctx itemid = do
    loandata <- fetchLoanIdWithItemId ctx itemid
    case loandata of
      Just _ -> pure Loaned
      Nothing -> pure NotLoaned

fetchLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> IdMap P.Employee -> LoanId -> m (Maybe Loan)
fetchLoan ctx emap lid = do
    runMaybeT $ do
        l <- MaybeT $ loan ctx lid
        b <- MaybeT $ fetchItem ctx $ ldItemId l
        binfoid <- MaybeT $ pure $ bookInformation b
        binfo <- MaybeT $ fetchBookInformation ctx binfoid
        pure $ loan' l binfo $ idLibrary b
  where
      bookInformation item =
          case idInfoId item of
              BookInfoId info -> Just info
              _               -> Nothing
      loan' (LoanData _ date personio_id itemId) bookInfo lib =
          Loan lid (T.pack $ show date) (Item itemId lib $ ItemBook bookInfo) (emap ^.at personio_id)

fetchLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> IdMap P.Employee -> m [Loan]
fetchLoans ctx es = do
    l <- loans ctx
    loanDataToLoan ctx es l

returnItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> LoanId -> m Bool
returnItem ctx lid = do
    l <- loan ctx lid
    case l of
      Nothing -> pure False
      Just (LoanData _ date (P.EmployeeId pid) iid) -> do
          _ <- safePoolExecute ctx "INSERT INTO library.old_loan (oldloan_id, date_loaned, date_returned, personio_id, item_id) VALUES (?,?,NOW(),?,?)" (lid, date, pid, iid)
          result <- safePoolExecute ctx "DELETE FROM library.loan WHERE loan_id = ?" (Only lid)
          if result == 1 then pure True else pure False

fetchPersonalLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> IdMap P.Employee -> P.EmployeeId -> m [Loan]
fetchPersonalLoans ctx es (P.EmployeeId eid) = do
    ldatas <- safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan WHERE personio_id = ?" (Only eid)
    loanDataToLoan ctx es ldatas

loanDataToLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => (Pool Connection) -> IdMap P.Employee -> [LoanData] -> m [Loan]
loanDataToLoan ctx es ldatas = do
    itemmap <- Map.fromList . fmap (\x -> (idItemId x, x)) <$> (fetchItemsWithItemIds ctx $ ldItemId <$> ldatas)
    bookInfos <- idMapOf folded <$> fetchBookInformations ctx
    boardgameInfos <- idMapOf folded <$> fetchBoardGameInformations ctx
    pure $ catMaybes $ (\l ->
        case (itemmap ^.at (ldItemId l)) >>= toItem bookInfos boardgameInfos of
            Just item -> Just $ Loan (ldLoanId l) (T.pack $ show $ ldDateLoaned l) item (es ^.at (ldPersonioId l))
            Nothing   -> Nothing) <$> ldatas
  where
    toItem binfos boainfos item = Item (idItemId item) (idLibrary item) <$>
        case idInfoId item of
          BookInfoId i      -> ItemBook      <$> binfos ^.at i
          BoardGameInfoId i -> ItemBoardGame <$> boainfos ^.at i

-------------------------------------------------------------------------------
-- Book functions
-------------------------------------------------------------------------------

fetchBookInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [BookInformation]
fetchBookInformations ctx = safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation" ()

fetchCoverInformationsAsText :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [(BookInformationId, Text)]
fetchCoverInformationsAsText ctx = safePoolQuery ctx "SELECT bookinfo_id, cover FROM library.bookinformation" ()

selectStatement :: SortCriteria -> Query
selectStatement (BookSort _) = "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation "
selectStatement (BoardGameSort _) = "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation "

startDirection :: SortDirection -> Query
startDirection SortDesc = " < "
startDirection SortAsc  = " > "

sortDirection :: SortDirection -> Query
sortDirection SortDesc = "DESC"
sortDirection SortAsc  = "ASC"

startPos :: SortCriteria -> SortDirection -> Query
startPos (BookSort SortTitle) dir         = " (title, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortAuthor) dir        = " (author, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortISBN) dir          = " (isbn, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortPublished) dir     = " (publishedYear, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortName) dir     = " (name, boardgameinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortDesigner) dir = " (designer, boardgameinfo_id)" <> startDirection dir <> "(?,?) "

sortCriteria :: SortCriteria -> SortDirection -> Query
sortCriteria (BookSort SortTitle) dir         = " ORDER BY title " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortAuthor) dir        = " ORDER BY author " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortISBN) dir          = " ORDER BY isbn " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortPublished) dir     = " ORDER BY publishedYear " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BoardGameSort SortName) dir     = " ORDER BY name " <> sortDirection dir <> ", boardgameinfo_id "
sortCriteria (BoardGameSort SortDesigner) dir = " ORDER BY designer " <> sortDirection dir <> ", boardgameinfo_id "

startBookInfo :: BookSortCriteria -> BookInformation -> Text
startBookInfo SortTitle info = info ^. bookTitle
startBookInfo SortAuthor info = info ^. bookAuthor
startBookInfo SortISBN info = info ^. bookISBN
startBookInfo SortPublished info = T.pack $ show $ info ^. bookPublished

startBoardGameInfo :: BoardGameSortCriteria -> BoardGameInformation -> Text
startBoardGameInfo SortName info = info ^. boardGameName
startBoardGameInfo SortDesigner info = fromMaybe "" $ info ^. boardGameDesigner

searchSqlString :: SortCriteria -> Query
searchSqlString (BookSort _) = " bookinfo_id in (SELECT bookinfo_id FROM (SELECT bookinfo_id, title, to_tsvector(title) || to_tsvector(isbn) || to_tsvector(author) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) as document FROM library.bookinformation) book_search WHERE book_search.document @@ plainto_tsquery(?)) "
searchSqlString (BoardGameSort _) = " boardgameinfo_id in (SELECT boardgameinfo_id FROM (SELECT boardgameinfo_id, name, to_tsvector(name) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) || to_tsvector(designer) || to_tsvector(artist) as document FROM library.boardgameinformation) boardgame_search WHERE boardgame_search.document @@ plainto_tsquery(?)) "

librarySqlString :: SortCriteria -> Query
librarySqlString (BookSort _) = " bookinfo_id in (select bookinfo_id from library.item where library = ? and bookinfo_id IS NOT NULL) "
librarySqlString (BoardGameSort _) = " boardgameinfo_id in (select boardgameinfo_id from library.item where library = ? and boardgameinfo_id IS NOT NULL) "

availabilitySqlString :: SortCriteria -> Maybe Library -> Query
availabilitySqlString (BookSort _) Nothing = " EXISTS (select * from library.item where bookinfo_id = library.bookinformation.bookinfo_id and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BookSort _) (Just _) = " EXISTS (select * from library.item where bookinfo_id = library.bookinformation.bookinfo_id and library = ? and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BoardGameSort _) Nothing = " EXISTS (select * from library.item where boardgameinfo_id = library.boardgameinformation.boardgameinfo_id and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BoardGameSort _) (Just _) = " EXISTS (select * from library.item where boardgameinfo_id = library.boardgameinformation.boardgameinfo_id and library = ? and item_id not in (select item_id from library.loan)) "

fetchInformationsWithCriteria :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m, FromRow a)
                                 => Pool Connection
                                 -> SortCriteriaAndStart
                                 -> SortDirection
                                 -> Int
                                 -> Maybe Text
                                 -> LibraryOrAll
                                 -> Bool
                                 -> m [a]
fetchInformationsWithCriteria ctx criteria direction limit search library onlyAvailable = case criteria of
    BookCS crit (Just bookInfo) -> fetchInfo (BookSort crit) (Just (bookInfo ^. bookInformationId, startBookInfo crit bookInfo))
    BookCS crit Nothing -> fetchInfo (BookSort crit) (Nothing :: Maybe (BookInformationId, Text))
    BoardGameCS crit (Just boardGameInfo) -> fetchInfo (BoardGameSort crit) (Just (boardGameInfo ^. boardGameInformationId, startBoardGameInfo crit boardGameInfo))
    BoardGameCS crit Nothing -> fetchInfo (BoardGameSort crit) (Nothing :: Maybe (BoardGameInformationId, Text))
  where
    fetchInfo :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m, FromRow a, ToField c, ToField d) => SortCriteria -> Maybe (c, d) -> m [a]
    fetchInfo crit info = case (info, search, library, onlyAvailable) of
      (Just (infoid, ii), Nothing, AllLibraries, False) -> safePoolQuery
                              ctx
                              ((selectStatement crit <> " WHERE " <> startPos crit direction <> sortCriteria crit direction) <> " LIMIT ?")
                              $ (ii, infoid, limit)
      (Just (infoid, ii), Just s, AllLibraries, False) -> safePoolQuery
                             ctx
                             ((selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> searchSqlString crit <> sortCriteria crit direction) <> " LIMIT ?")
                             $ (ii, infoid, s, limit)
      (Nothing, Nothing, AllLibraries, False) -> safePoolQuery ctx (selectStatement crit <> sortCriteria crit direction <> " LIMIT ?") (Only limit)
      (Nothing, Just s, AllLibraries, False) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> searchSqlString crit <> sortCriteria crit direction <> " LIMIT ?") (s, limit)
      (Just (infoid, ii), Nothing, JustLibrary lib, False) -> safePoolQuery
                              ctx
                              ((selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> librarySqlString crit <> sortCriteria crit direction) <> " LIMIT ?")
                              $ (ii, infoid, lib, limit)
      (Just (infoid, ii), Just s, JustLibrary lib, False) -> safePoolQuery
                             ctx
                             ((selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> searchSqlString crit <> " AND " <> librarySqlString crit <> sortCriteria crit direction) <> " LIMIT ?")
                             $ (ii, infoid, s, lib, limit)
      (Nothing, Nothing, JustLibrary lib, False) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?") (lib, limit)
      (Nothing, Just s, JustLibrary lib, False) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> searchSqlString crit <> " AND " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?") (s, lib, limit)
      (Just (infoid, ii), Nothing, AllLibraries, True) -> safePoolQuery
                              ctx
                              (selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> availabilitySqlString crit Nothing <> sortCriteria crit direction <> " LIMIT ?")
                              $ (ii, infoid, limit)
      (Just (infoid, ii), Just s, AllLibraries, True) -> safePoolQuery
                             ctx
                             (selectStatement crit <> " WHERE " <> wherewrapper [startPos crit direction, searchSqlString crit, availabilitySqlString crit Nothing] <> sortCriteria crit direction <> " LIMIT ?")
                             $ (ii, infoid, s, limit)
      (Nothing, Nothing, AllLibraries, True) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> availabilitySqlString crit Nothing <> sortCriteria crit direction <> " LIMIT ?") (Only limit)
      (Nothing, Just s, AllLibraries, True) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> wherewrapper [searchSqlString crit, availabilitySqlString crit Nothing] <> sortCriteria crit direction <> " LIMIT ?") (s, limit)
      (Just (infoid, ii), Nothing, JustLibrary lib, True) -> safePoolQuery
                              ctx
                              ((selectStatement crit <> " WHERE " <> wherewrapper [startPos crit direction,librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction) <> " LIMIT ?")
                              $ (ii, infoid, lib, lib, limit)
      (Just (infoid, ii), Just s, JustLibrary lib, True) -> safePoolQuery
                             ctx
                             ((selectStatement crit <> " WHERE " <> wherewrapper [startPos crit direction, searchSqlString crit, librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction) <> " LIMIT ?")
                             $ (ii, infoid, s, lib, lib, limit)
      (Nothing, Nothing, JustLibrary lib, True) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> wherewrapper [librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?") (lib, lib, limit)
      (Nothing, Just s, JustLibrary lib, True) -> safePoolQuery ctx (selectStatement crit <> " WHERE " <> wherewrapper [searchSqlString crit, librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?") (s, lib, lib, limit)
    wherewrapper = fold . intersperse " AND "


fetchBookInformationByISBN :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> Text -> m (Maybe BookInformationResponse)
fetchBookInformationByISBN ctx isbn = do
    binfoid <- safePoolQuery ctx "SELECT bookinfo_id FROM library.bookinformation WHERE isbn = ?" (Only isbn)
    case listToMaybe binfoid of
      Just infoid -> listToMaybe <$> fetchBookResponse ctx infoid
      Nothing -> pure Nothing

fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BookInformationId -> m (Maybe BookInformation)
fetchBookInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation WHERE bookinfo_id = ?" (Only binfoid)

fetchBookResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BookInformationId -> m [BookInformationResponse]
fetchBookResponse ctx binfoid = do
    books <- fetchItemsByBookInformation ctx binfoid
    binfo <- fetchBookInformation ctx binfoid
    case binfo of
      Nothing -> pure $ []
      Just info -> pure $ [toBookInformationResponse (fold (toBooks <$> books)) info]
  where
      toBooks item = [Books (idLibrary item) (idItemId item)]
      toBookInformationResponse books (BookInformation infoid title isbn author publisher published cover amazonLink) =
          BookInformationResponse infoid title isbn author publisher published cover amazonLink books

fetchBoardGameResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BoardGameInformationId -> m (Maybe BoardGameInformationResponse)
fetchBoardGameResponse ctx infoid = do
    boardgames <- fetchItemsByBoardGameInformationId ctx infoid
    binfo <- fetchBoardGameInformation ctx infoid
    case binfo of
      Nothing -> pure Nothing
      Just info -> pure $ pure $ toBoardGameInformationResponse info (toBoardGame <$> boardgames)
  where
    toBoardGame item = BoardGames (idLibrary item) (idItemId item)
    toBoardGameInformationResponse info boardgames = BoardGameInformationResponse
        { _boardGameResponseInformationId  = info ^. boardGameInformationId
        , _boardGameResponseName           = info ^. boardGameName
        , _boardGameResponsePublisher      = info ^. boardGamePublisher
        , _boardGameResponsePublished      = info ^. boardGamePublished
        , _boardGameResponseDesigner       = info ^. boardGameDesigner
        , _boardGameResponseArtist         = info ^. boardGameArtist
        , _boardGameResponseGames          = boardgames
        }

fetchBooksResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> [BookInformation] -> m [BookInformationResponse]
fetchBooksResponse ctx bookInfos = do
    books <- fetchItems ctx
    pure $ (toBookInformationResponse (bookInformationMap books)) <$> bookInfos
  where
      bookInformationMap books = Map.fromListWith (++) $ catMaybes (toBooks <$> books)
      toBooks (ItemData iid lib bookinfoid) =
          case bookinfoid of
              BookInfoId binfoid -> Just (binfoid, [Books lib iid])
              _                  -> Nothing
      toBookInformationResponse books (BookInformation binfoid title isbn author publisher published cover amazonLink) =
          BookInformationResponse binfoid title isbn author publisher published cover amazonLink (fromMaybe [] (books ^.at binfoid))

bookIdToInformation :: ItemId -> Map ItemId (Maybe BookInformationId, Library) -> Map BookInformationId BookInformation -> Maybe BookInformation
bookIdToInformation iid bookidmap bookinfomap = do
    bookinfoid <- fst <$> (bookidmap ^.at iid)
    binfoid <- bookinfoid
    bookinfomap ^.at binfoid

checkIfExistingBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> Maybe BookInformationId -> Text -> m (Maybe BookInformationId)
checkIfExistingBook ctx binfoid isbn = runMaybeT $ do
    info <- MaybeT $ pure binfoid
    bookInformation <- MaybeT $ fetchBookInformation ctx info
    if bookInformation ^. bookISBN == isbn then pure $ bookInformation ^. bookInformationId else MaybeT $ pure Nothing

addNewBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> AddBookInformation -> Maybe ContentHash -> m Bool
addNewBook ctx (AddBookInformation title isbn author publisher published amazonLink libraryBooks _ binfoid) contentHash = do
    checkedInfoId <- checkIfExistingBook ctx binfoid isbn
    bookInfoId <- case checkedInfoId of
                    Nothing -> runMaybeT $ do
                        ch <- pure contentHash
                        MaybeT $ listToMaybe <$> safePoolQuery
                            ctx
                            "INSERT INTO library.bookinformation (title, isbn, author, publisher, publishedyear, cover, amazon_link) VALUES (?,?,?,?,?,?,?) returning bookinfo_id"
                            (title, isbn, author, publisher, published, ch, amazonLink)
                    Just info -> pure $ Just info
    case bookInfoId of
      Just infoId -> do
          for_ libraryBooks $ \(lib, n) -> do
              replicateM_ n $ safePoolExecute ctx "INSERT INTO library.item (library, bookinfo_id) VALUES (?,?)" (lib, infoId :: BookInformationId)
          pure True
      Nothing -> pure False

updateBookCover :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BookInformationId -> ContentHash -> m Int64
updateBookCover ctx binfoid contentHash = safePoolExecute ctx "UPDATE library.bookinformation SET cover = ? where bookinfo_id = ?" (contentHash, binfoid)

updateBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> EditBookInformation -> m Int64
updateBookInformation ctx info = safePoolExecute ctx "UPDATE library.bookinformation SET title = ?, isbn = ?, author = ?, publisher = ?, publishedyear = ?, amazon_link = ? where bookinfo_id = ?" (info ^. editBookTitle, info ^. editBookISBN, info ^. editBookAuthor, info ^. editBookPublisher, info ^. editBookPublished, info ^. editBookAmazonLink, info ^. editBookInformationId)

-------------------------------------------------------------------------------
-- Boardgame functions
-------------------------------------------------------------------------------

fetchBoardGameInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [BoardGameInformation]
fetchBoardGameInformations ctx = safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation" ()

fetchBoardGameInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> BoardGameInformationId -> m (Maybe BoardGameInformation)
fetchBoardGameInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation WHERE boardgameinfo_id = ?" (Only binfoid)

addNewBoardGame :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> AddBoardGameInformation -> m Bool
addNewBoardGame ctx (AddBoardGameInformation name publisher published designer artist libraryBoardgames) = do
    boardgameInfoId <- listToMaybe <$> safePoolQuery
                       ctx
                       "INSERT INTO library.boardgameinformation (name, publisher, publishedyear, designer, artist) VALUES (?,?,?,?,?) returning boardgameinfo_id"
                       (name, publisher, published, designer, artist)
    case boardgameInfoId of
      Just infoId -> do
          for_ libraryBoardgames $ \(lib, n) -> do
              replicateM_ n $ safePoolExecute ctx "INSERT INTO library.item (library, boardgameinfo_id) VALUES (?,?)" (lib, infoId :: BookInformationId)
          pure True
      Nothing -> pure False

updateBoardGameInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> EditBoardGameInformation -> m Int64
updateBoardGameInformation ctx info = safePoolExecute ctx "UPDATE library.boardgameinformation SET name = ?, publisher = ?, publishedyear = ?, designer = ?, artist = ? WHERE boardgameinfo_id = ?" (info ^. editBoardGameName, info ^. editBoardGamePublisher, info ^. editBoardGamePublished, info ^. editBoardGameDesigner, info ^. editBoardGameArtist, info ^. editBoardGameInformationId)
