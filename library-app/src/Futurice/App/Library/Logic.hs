{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic where

import Data.List
import Database.PostgreSQL.Simple         (In (..))
import Database.PostgreSQL.Simple.FromRow
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
    { idItemId                 :: !ItemId
    , idLibrary                :: !Library
    , idInfoId                 :: !InfoId
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
        itemId <- field
        library <- field
        bookInfo <- field
        boardGameInfo <- field
        case BookInfoId <$> bookInfo <|> BoardGameInfoId <$> boardGameInfo of
            Just infoid -> pure $ ItemData itemId library infoid
            Nothing     -> empty

-------------------------------------------------------------------------------
-- Logic functions
-------------------------------------------------------------------------------

idToName :: IdMap P.Employee -> P.EmployeeId -> Text
idToName emap pid = case emap ^.at pid of
  Just employee -> (employee ^. P.employeeFirst) <> " " <> (employee ^. P.employeeLast)
  Nothing -> "Unknown"

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

fetchBooksByBookInformation :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [ItemData]
fetchBooksByBookInformation ctx binfoid = safePoolQuery ctx "select item_id, library, bookinfo_id, boardgameinfo_id from library.item where bookinfo_id = ?" (Only binfoid)

fetchLoansWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemId] -> m [LoanData]
fetchLoansWithItemIds ctx iids = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where item_id in ?;" (Only (In iids))

fetchItemsWithItemIds :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemId] -> m [ItemData]
fetchItemsWithItemIds ctx iids = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id in ?;" (Only (In iids))

fetchItemsWithoutLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> [ItemData] -> m (Maybe ItemId)
fetchItemsWithoutLoans ctx items = do
    ls <- fetchLoansWithItemIds ctx iids
    pure $ listToMaybe $ iids \\ (ldItemId <$> ls)
  where
    iids = idItemId <$> items

borrowBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> P.EmployeeId -> BorrowRequest -> m (Maybe LoanData)
borrowBook ctx eid (BorrowRequest binfoid _) = do
    now <- currentDay
    books <- fetchBooksByBookInformation ctx binfoid
    runMaybeT $ do
        freeBook <- MaybeT $ fetchItemsWithoutLoans ctx books
        MaybeT $ makeLoan ctx now freeBook eid

snatchBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> P.EmployeeId -> ItemId -> m (Maybe LoanData)
snatchBook ctx eid iid = do
    now <- currentDay
    runMaybeT $ do
        (LoanData lid _ _ _) <- MaybeT $ fetchLoanByItemId ctx iid
        MaybeT $ makeForcedLoan ctx now lid eid

fetchBookInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformation]
fetchBookInformations ctx = safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation" ()

fetchBoardGameInformations :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BoardGameInformation]
fetchBoardGameInformations ctx = safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation" ()

fetchBookInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m (Maybe BookInformation)
fetchBookInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation WHERE bookinfo_id = ?" (Only binfoid)

fetchBoardGameInformation :: (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BoardGameInformationId -> m (Maybe BoardGameInformation)
fetchBoardGameInformation ctx binfoid = listToMaybe <$> safePoolQuery ctx "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation WHERE boardgameinfo_id = ?" (Only binfoid)

fetchBookResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> BookInformationId -> m [BookInformationResponse]
fetchBookResponse ctx binfoid = do
    books <- fetchBooksByBookInformation ctx binfoid
    binfo <- fetchBookInformation ctx binfoid
    case binfo of
      Nothing -> pure $ []
      Just info -> pure $ [toBookInformationResponse (fold (toBooks <$> books)) info]
  where
      toBooks item = [Books (idItemId item) (idLibrary item)]
      toBookInformationResponse books (BookInformation infoid title isbn author publisher published cover amazonLink) =
          BookInformationResponse infoid title isbn author publisher published cover amazonLink books

fetchBooksResponse :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [BookInformationResponse]
fetchBooksResponse ctx = do
    books <- fetchItems ctx
    bookInfos <- fetchBookInformations ctx
    pure $ (toBookInformationResponse (bookInformationMap books)) <$> bookInfos
  where
      bookInformationMap books = Map.fromListWith (++) $ catMaybes (toBooks <$> books)
      toBooks (ItemData iid lib bookinfoid) =
          case bookinfoid of
              BookInfoId binfoid -> Just (binfoid, [Books iid lib])
              _                  -> Nothing
      toBookInformationResponse books (BookInformation binfoid title isbn author publisher published cover amazonLink) =
          BookInformationResponse binfoid title isbn author publisher published cover amazonLink (fromMaybe [] (books ^.at binfoid))

fetchItem :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> ItemId -> m (Maybe ItemData)
fetchItem ctx iid = listToMaybe <$> (safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item WHERE item_id = ?" $ Only iid)

fetchItems :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [ItemData]
fetchItems ctx = safePoolQuery ctx "SELECT item_id, library, bookinfo_id, boardgameinfo_id FROM library.item " ()

loans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [LoanData]
loans ctx = safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan" ()

loan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m (Maybe LoanData)
loan ctx lid = listToMaybe <$> safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan where loan_id = ?" (Only lid)

fetchLoan :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> IdMap P.Employee -> m (Maybe Loan)
fetchLoan ctx lid es = do
    runMaybeT $ do
        l <- MaybeT $ loan ctx lid
        b <- MaybeT $ fetchItem ctx $ ldItemId l
        binfoid <- MaybeT $ pure $ bookInformation b
        binfo <- MaybeT $ fetchBookInformation ctx binfoid
        pure $ loan' l es binfo $ idLibrary b
  where
      bookInformation item =
          case idInfoId item of
              BookInfoId info -> Just info
              _               -> Nothing
      loan' (LoanData loanId date personio_id itemId) emap bookInfo lib =
          Loan loanId (T.pack $ show date) (Item itemId lib $ ItemBook bookInfo) (emap ^.at personio_id)

itemArrayToMap :: [ItemData] -> Map ItemId (InfoId, Library)
itemArrayToMap = Map.fromList . fmap (\(ItemData iid lib infoid) -> (iid, (infoid, lib)))

bookIdToInformation :: ItemId -> Map ItemId (Maybe BookInformationId, Library) -> Map BookInformationId BookInformation -> Maybe BookInformation
bookIdToInformation iid bookidmap bookinfomap = do
    bookinfoid <- fst <$> (bookidmap ^.at iid)
    binfoid <- bookinfoid
    bookinfomap ^.at binfoid

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

fetchLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> m [Loan]
fetchLoans ctx es = do
    l <- loans ctx
    loanDataToLoan ctx es l

executeReturnBook :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> LoanId -> m Bool
executeReturnBook ctx lid = do
    result <- safePoolExecute ctx "DELETE FROM library_loan WHERE id = ?" (Only lid)
    if result == 1 then pure True else pure False

fetchPersonalLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee -> P.EmployeeId -> m [Loan]
fetchPersonalLoans ctx es (P.EmployeeId eid) = do
    ldatas <- safePoolQuery ctx "SELECT loan_id, date_loaned, personio_id, item_id FROM library.loan WHERE personio_id = ?" (Only eid)
    loanDataToLoan ctx es ldatas
