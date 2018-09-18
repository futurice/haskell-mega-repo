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

import Futurice.App.Library.Ctx
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

fetchItemsWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemId] -> m [ItemData]
fetchItemsWithItemIds ctx iids = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id in ?;" (Only (In iids))

fetchItemsWithoutLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemData] -> m (Maybe ItemId)
fetchItemsWithoutLoans ctx items = do
    ls <- fetchLoansWithItemIds ctx iids
    pure $ listToMaybe $ iids \\ (ldItemId <$> ls)
  where
    iids = idItemId <$> items

fetchItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> ItemId -> m (Maybe ItemData)
fetchItem ctx iid = listToMaybe <$> (safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id = ?" $ Only iid)

fetchItems :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [ItemData]
fetchItems ctx = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item " ()

-------------------------------------------------------------------------------
-- Loan functions
-------------------------------------------------------------------------------

fetchLoanByItemId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> ItemId -> m (Maybe LoanData)
fetchLoanByItemId ctx iid = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id = ?" (Only iid)

makeLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Day -> ItemId -> P.EmployeeId -> m (Maybe LoanData)
makeLoan ctx date iid (P.EmployeeId eid) = do
    _ <- safePoolExecute ctx "INSERT INTO library.loan (date_loaned, personio_id, item_id) VALUES (?,?,?)" (date, toInteger eid, iid)
    listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id = ?" (Only iid)

makeForcedLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Day -> LoanId -> P.EmployeeId -> m (Maybe LoanData)
makeForcedLoan ctx date lid (P.EmployeeId eid) = do
    result <- safePoolExecute ctx "UPDATE library.loan SET date_loaned = ?, personio_id = ? WHERE loan_id = ?" (date, toInteger eid, lid)
    if result == 1 then
      listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where loan_id = ?" (Only lid)
    else
      pure Nothing

fetchLoansWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemId] -> m [LoanData]
fetchLoansWithItemIds ctx iids = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id in ?;" (Only (In iids))

borrowBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> P.EmployeeId -> BorrowRequest -> m (Maybe LoanData)
borrowBook ctx eid (BorrowRequest binfoid _) = do
    now <- currentDay
    books <- fetchItemsByBookInformation ctx binfoid
    runMaybeT $ do
        freeBook <- MaybeT $ fetchItemsWithoutLoans ctx books
        MaybeT $ makeLoan ctx now freeBook eid

snatchBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> P.EmployeeId -> ItemId -> m (Maybe LoanData)
snatchBook ctx eid iid = do
    now <- currentDay
    runMaybeT $ do
        (LoanData lid _ _ _) <- MaybeT $ fetchLoanByItemId ctx iid
        MaybeT $ makeForcedLoan ctx now lid eid

loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [LoanData]
loans ctx = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan" ()

loan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m (Maybe LoanData)
loan ctx lid = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where loan_id = ?" (Only lid)

fetchLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> LoanId -> m (Maybe Loan)
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

fetchLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> m [Loan]
fetchLoans ctx es = do
    l <- loans ctx
    loanDataToLoan ctx es l

executeReturnBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m Bool
executeReturnBook ctx lid = do
    l <- loan ctx lid
    case l of
      Nothing -> pure False
      Just (LoanData _ date (P.EmployeeId pid) iid) -> do
          _ <- safePoolExecute ctx "INSERT INTO library.old_loan (oldloan_id, date_loaned, date_returned, personio_id, item_id) VALUES (?,?,NOW(),?,?)" (lid, date, pid, iid)
          result <- safePoolExecute ctx "DELETE FROM library.loan WHERE loan_id = ?" (Only lid)
          if result == 1 then pure True else pure False

fetchPersonalLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> P.EmployeeId -> m [Loan]
fetchPersonalLoans ctx es (P.EmployeeId eid) = do
    ldatas <- safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan WHERE personio_id = ?" (Only eid)
    loanDataToLoan ctx es ldatas

loanDataToLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> [LoanData] -> m [Loan]
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

fetchItemsByBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [ItemData]
fetchItemsByBookInformation ctx binfoid = safePoolQuery ctx "select item_id, library, bookinfo_id, boardgameinfo_id from library.item where bookinfo_id = ?" (Only binfoid)

fetchItemsByBoardGameInformationId :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BoardGameInformationId -> m [ItemData]
fetchItemsByBoardGameInformationId ctx binfoid = safePoolQuery ctx "select item_id, library, bookinfo_id, boardgameinfo_id from library.item where boardgameinfo_id = ?" (Only binfoid)

-------------------------------------------------------------------------------
-- Book functions
-------------------------------------------------------------------------------

fetchBookInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformation]
fetchBookInformations ctx = safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation" ()

fetchCoverInformationsAsText :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [(BookInformationId, Text)]
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
startPos (BookSort SortTitle) dir         = " WHERE (title, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortAuthor) dir        = " WHERE (author, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortISBN) dir          = " WHERE (isbn, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortPublished) dir     = " WHERE (publishedYear, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortName) dir     = " WHERE (name, boardgameinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortDesigner) dir = " WHERE (designer, boardgameinfo_id)" <> startDirection dir <> "(?,?) "

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
startBoardGameInfo SortName info = info ^. boardgameName
startBoardGameInfo SortDesigner info = fromMaybe "" $ info ^. boardgameDesigner

searchSqlString :: SortCriteria -> Query
searchSqlString (BookSort _) = " bookinfo_id in (SELECT bookinfo_id FROM (SELECT bookinfo_id, title, to_tsvector(title) || to_tsvector(isbn) || to_tsvector(author) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) as document FROM library.bookinformation) book_search WHERE book_search.document @@ plainto_tsquery(?)) "
searchSqlString (BoardGameSort _) = " boardgameinfo_id in (SELECT boardgameinfo_id FROM (SELECT boardgameinfo_id, name, to_tsvector(name) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) || to_tsvector(designer) || to_tsvector(artist) as document FROM library.boardgameinformation) boardgame_search WHERE boardgame_search.document @@ plainto_tsquery(?)) "

fetchInformationsWithCriteria :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m, FromRow a)
                                 => Ctx
                                 -> SortCriteriaAndStart
                                 -> SortDirection
                                 -> Int
                                 -> Maybe Text
                                 -> m [a]
fetchInformationsWithCriteria ctx criteria direction limit search = case criteria of
    BookCS crit start -> do
        bookInfo <- join <$> traverse (fetchBookInformation ctx) start
        case bookInfo of
            Just bi -> fetchInfo (BookSort crit) (Just (bi ^. bookInformationId, startBookInfo crit bi))
            Nothing -> fetchInfo (BookSort crit) (Nothing :: Maybe (BookInformationId, Text))
    BoardGameCS crit start -> do
        boardGameInfo <- join <$> traverse (fetchBoardGameInformation ctx) start
        case boardGameInfo of
            Just bi -> fetchInfo (BoardGameSort crit) (Just (bi ^. boardgameInformationId, startBoardGameInfo crit bi))
            Nothing -> fetchInfo (BoardGameSort crit) (Nothing :: Maybe (BoardGameInformationId, Text))
  where
    fetchInfo :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m, FromRow a, ToField c, ToField d) => SortCriteria -> Maybe (c, d) -> m [a]
    fetchInfo crit info = case (info, search) of
      (Just (infoid, ii), Nothing) -> safePoolQuery
                              ctx
                              ((selectStatement crit <> startPos crit direction <> sortCriteria crit direction) <> " LIMIT ?")
                              $ (ii, infoid, limit)
      (Just (infoid, ii), Just s) -> safePoolQuery
                             ctx
                             ((selectStatement crit <> startPos crit direction <> " AND " <> searchSqlString crit <> sortCriteria crit direction) <> " LIMIT ?")
                             $ (ii, infoid, s, limit)
      (Nothing, Nothing)  -> safePoolQuery ctx (selectStatement crit <> sortCriteria crit direction <> " LIMIT ?") (Only limit)
      (Nothing, Just s)   -> safePoolQuery ctx (selectStatement crit <> "WHERE" <> searchSqlString crit <> sortCriteria crit direction <> " LIMIT ?") (s, limit)

fetchBookInformationByISBN :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Text -> m (Maybe BookInformationResponse)
fetchBookInformationByISBN ctx isbn = do
    binfoid <- safePoolQuery ctx "SELECT bookinfo_id FROM library.bookinformation WHERE isbn = ?" (Only isbn)
    case listToMaybe binfoid of
      Just infoid -> listToMaybe <$> fetchBookResponse ctx infoid
      Nothing -> pure Nothing

fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m (Maybe BookInformation)
fetchBookInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation WHERE bookinfo_id = ?" (Only binfoid)

fetchBookResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [BookInformationResponse]
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

fetchBoardGameResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BoardGameInformationId -> m (Maybe BoardGameInformationResponse)
fetchBoardGameResponse ctx infoid = do
    boardgames <- fetchItemsByBoardGameInformationId ctx infoid
    binfo <- fetchBoardGameInformation ctx infoid
    case binfo of
      Nothing -> pure Nothing
      Just info -> pure $ pure $ toBoardGameInformationResponse info (toBoardGame <$> boardgames)
  where
    toBoardGame item = BoardGames (idLibrary item) (idItemId item)
    toBoardGameInformationResponse info boardgames = BoardGameInformationResponse
        { _boardgameResponseInformationId  = info ^. boardgameInformationId
        , _boardgameResponseName           = info ^. boardgameName
        , _boardgameResponsePublisher      = info ^. boardgamePublisher
        , _boardgameResponsePublished      = info ^. boardgamePublished
        , _boardgameResponseDesigner       = info ^. boardgameDesigner
        , _boardgameResponseArtist         = info ^. boardgameArtist
        , _boardgameResponseGames          = boardgames
        }

fetchBooksResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [BookInformation] -> m [BookInformationResponse]
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

checkIfExistingBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> Maybe BookInformationId -> Text -> m (Maybe BookInformationId)
checkIfExistingBook ctx binfoid isbn = runMaybeT $ do
    info <- MaybeT $ pure binfoid
    bookInformation <- MaybeT $ fetchBookInformation ctx info
    if bookInformation ^. bookISBN == isbn then pure $ bookInformation ^. bookInformationId else MaybeT $ pure Nothing

addNewBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> AddBookInformation -> Maybe ContentHash -> m Bool
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

updateBookCover :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> ContentHash -> m Int64
updateBookCover ctx binfoid contentHash = safePoolExecute ctx "UPDATE library.bookinformation SET cover = ? where bookinfo_id = ?" (contentHash, binfoid)

updateBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> EditBookInformation -> m Int64
updateBookInformation ctx info = safePoolExecute ctx "UPDATE library.bookinformation SET title = ?, isbn = ?, author = ?, publisher = ?, publishedyear = ?, amazon_link = ? where bookinfo_id = ?" (info ^. editBookTitle, info ^. editBookISBN, info ^. editBookAuthor, info ^. editBookPublisher, info ^. editBookPublished, info ^. editBookAmazonLink, info ^. editBookInformationId)

-------------------------------------------------------------------------------
-- Boardgame functions
-------------------------------------------------------------------------------

fetchBoardGameInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BoardGameInformation]
fetchBoardGameInformations ctx = safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation" ()

fetchBoardGameInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BoardGameInformationId -> m (Maybe BoardGameInformation)
fetchBoardGameInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation WHERE boardgameinfo_id = ?" (Only binfoid)

addNewBoardGame :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> AddBoardGameInformation -> m Bool
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

updateBoardGameInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> EditBoardGameInformation -> m Int64
updateBoardGameInformation ctx info = safePoolExecute ctx "UPDATE library.boardgameinformation SET name = ?, publisher = ?, publishedyear = ?, designer = ?, artist = ? WHERE boardgameinfo_id = ?" (info ^. editBoardGameName, info ^. editBoardGamePublisher, info ^. editBoardGamePublished, info ^. editBoardGameDesigner, info ^. editBoardGameArtist, info ^. editBoardGameInformationId)
