{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic (
    module Futurice.App.Library.Logic,
    module Futurice.App.Library.Logic.Informations,
    ) where

import Control.Monad                      (replicateM_)
import Data.List
import Database.PostgreSQL.Simple         (In (..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Futurice.App.Sisosota.Types        (ContentHash)
import Futurice.IdMap                     (IdMap)
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types
import Futurice.App.Library.Logic.Informations

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
borrowBook ctx eid (BorrowRequest binfoid library) = do
    now <- currentDay
    books <- fetchItemsByBookInformation ctx binfoid
    runMaybeT $ do
        freeBook <- MaybeT $ fetchItemsWithoutLoans ctx (filter (\b -> library == idLibrary b) books)
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
    catMaybes <$> for ldatas (\ldata -> do
        runMaybeT $ do
            itemData <- MaybeT $ fetchItem ctx $ ldItemId ldata
            case idInfoId itemData of
              BookInfoId infoid -> do
                  i <- fetchBookInformation ctx infoid
                  item <- MaybeT $ pure $ (Item (idItemId itemData) (idLibrary itemData) . ItemBook) <$> i
                  pure $ Loan (ldLoanId ldata) (T.pack $ show $ ldDateLoaned ldata) item (es ^.at (ldPersonioId ldata))
              BoardGameInfoId infoid -> do
                  i <- fetchBoardGameInformation ctx infoid
                  item <- MaybeT $ pure $ (Item (idItemId itemData) (idLibrary itemData) . ItemBoardGame) <$> i
                  pure $ Loan (ldLoanId ldata) (T.pack $ show $ ldDateLoaned ldata) item (es ^.at (ldPersonioId ldata)))

-------------------------------------------------------------------------------
-- Book functions
-------------------------------------------------------------------------------

fetchBookInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [BookInformation]
fetchBookInformations ctx = safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation" ()

fetchCoverInformationsAsText :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Pool Connection -> m [(BookInformationId, Text)]
fetchCoverInformationsAsText ctx = safePoolQuery ctx "SELECT bookinfo_id, cover FROM library.bookinformation" ()

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
