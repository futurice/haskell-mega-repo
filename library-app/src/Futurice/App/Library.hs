{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.Library (defaultMain) where

import Codec.Picture                (DynamicImage, decodeImage)
import Control.Concurrent.STM
       (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Lens
import Data.Aeson                   (eitherDecode)
import Data.Char                    (isSpace)
import Data.Maybe                   (isJust)
import FUM.Types.Login
import Futurice.App.Sisosota.Client
import Futurice.App.Sisosota.Types  (ContentHash)
import Futurice.FUM.MachineAPI      (FUM6 (..), fum6)
import Futurice.IdMap               (IdMap)
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Client
import Servant.Multipart
import Servant.Server.Generic

import Futurice.App.Library.API
import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.Logic
import Futurice.App.Library.Markup
import Futurice.App.Library.Pages.AddItemPage
import Futurice.App.Library.Pages.BoardGameInformationPage
import Futurice.App.Library.Pages.BookInformationPage
import Futurice.App.Library.Pages.EditItemPage
import Futurice.App.Library.Pages.IndexPage
import Futurice.App.Library.Pages.PersonalLoansPage
import Futurice.App.Library.Reminder
import Futurice.App.Library.Types
import Futurice.App.Library.Types.GoogleBookResponse

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Futurice.IdMap       as IdMap
import qualified Network.HTTP.Client  as HTTP
import qualified Personio             as P

apiServer :: Ctx -> Server LibraryAPI
apiServer ctx = genericServer $ Record
    { booksGet               = getBooksImpl ctx
    , bookGet                = getBookImpl ctx
    , bookByISBNGet          = getBookByISBNImpl ctx
    , bookCoverGet           = getBookCoverImpl ctx
    , borrowPost             = borrowBookImpl ctx
    , borrowWithUserPost     = borrowWithUserPostImpl ctx
    , itemDelete             = itemDeleteImpl ctx
    , snatchPost             = snatchBookImpl ctx
    , loansGet               = getLoansImpl ctx
    , loanGet                = getLoanImpl ctx
    , returnPost             = returnLoanImpl ctx
    , personalLoansGet       = personalLoansImpl ctx
    , sendReminderEmailsPost = sendReminderEmailsImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { addBookPost          = addBookPostImpl ctx
    , addBoardGamePost     = addBoardGamePostImpl ctx
    , addItemPageGet       = addItemPageGetImpl ctx
    , addItemPost          = addItemPostImpl ctx
    , bookPageGet          = bookInformationPageImpl ctx
    , boardGamePageGet     = boardGameInformationPageImpl ctx
    , editBoardGamePost    = editBoardGameInformationImpl ctx
    , editBoardGamePageGet = editBoardGameInformationPageImpl ctx
    , editBookPageGet      = editBookInformationPageImpl ctx
    , editBookPost         = editBookInformationImpl ctx
    , indexPageGet         = indexPageImpl ctx
    , personalLoansPageGet = personalLoansPageImpl ctx
    }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ LibraryService
    & serverDescription      .~ "Futushelf 2.0"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp libraryApi   .~ apiServer
    & serverEnvPfx           .~ "LIBRARYAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache mq = do
    pp <- createPostgresPool $ cfgPostgresConnInfo cfg

    let fetchEmployees = do
            now <- currentTime
            IdMap.fromFoldable <$> runIntegrations mgr lgr now integrationConfig P.personioEmployees
    employees <- fetchEmployees

    ctx <- Ctx cfg pp lgr mgr cache <$> newTVarIO employees

    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every 3600

    -- listen to MQ, especially for library reminder requests
    void $ forEachMessage mq $ \msg -> case msg of
      LibraryReminderPing -> void $ do
          emps <- liftIO $ readTVarIO $ ctxPersonio ctx
          runLogT "send-reminder" (ctxLogger ctx) $ sendReminderEmails ctx emps
      _                   -> pure ()

    return (ctx, [employeesJob])
  where
    integrationConfig = cfgIntegrationsCfg cfg

    updateJob :: Ctx -> IO (IdMap P.Employee) -> IO ()
    updateJob ctx fetchEmployees = do
        employees <- fetchEmployees
        atomically $ writeTVar (ctxPersonio ctx) employees

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

getPersonioDataMap :: Ctx -> IO (Map Login P.Employee)
getPersonioDataMap ctx = do
    es' <- toList <$> (liftIO $ readTVarIO $ ctxPersonio ctx)
    pure $ Map.fromList $ catMaybes $ fmap (\e -> case e ^. P.employeeLogin of
                                               Just login -> Just (login, e)
                                               Nothing -> Nothing) es'

fetchCover :: (Monad m, MonadIO m, MonadThrow m) => Ctx -> ContentHash -> m (Either String LBS.ByteString)
fetchCover ctx contentHash = do
    url <- parseBaseUrl $ T.unpack $ cfgSisosotaUrl $ ctxConfig ctx
    liftIO $ sisosotaGet (ctxManager ctx) url contentHash

addNewCover :: Ctx -> LBS.ByteString -> IO ContentHash
addNewCover ctx cover = do
    url <- parseBaseUrl $ T.unpack $ cfgSisosotaUrl $ ctxConfig ctx
    sisosotaPut (ctxManager ctx) url cover

makeGoogleAddress :: Text -> String
makeGoogleAddress isbn = "https://www.googleapis.com/books/v1/volumes?q=isbn:" <> T.unpack isbn

fetchBookInformationFromGoogle :: Ctx -> Text -> IO (Maybe BookInformationByISBNResponse)
fetchBookInformationFromGoogle ctx isbn = runLogT "fetch-from-google" (ctxLogger ctx) $ do
    res <- liftIO $ fetchFromGoogle
    case res of
      Left err -> do
          logAttention_ $ "Error parsing Google Books response: " <> textShow err <> " with isbn " <> isbn
          pure Nothing
      Right book -> pure book
  where
    fetchFromGoogle :: IO (Either String (Maybe BookInformationByISBNResponse))
    fetchFromGoogle = runExceptT $ do
        let address = makeGoogleAddress isbn
        request <- liftIO $ HTTP.parseRequest address
        response <- liftIO $ HTTP.httpLbs request (ctxManager ctx)
        book <- ExceptT $ pure $ eitherDecode $ HTTP.responseBody response
        request' <- liftIO $ HTTP.parseRequest (T.unpack $ book ^. gbrSelfLink)
        response' <- liftIO $ HTTP.httpLbs request' (ctxManager ctx)
        idBook <- ExceptT $ pure $ eitherDecode $ HTTP.responseBody response'
        case book ^. gbrPublisher <|> idBook ^. gibrPublisher of
          Nothing -> ExceptT $ pure $ Left "No publisher found for book"
          Just p ->
              pure $ Just $ BookInformationByISBNResponse
              (book ^. gbrTitle)
              isbn
              (T.intercalate ", " $ book ^. gbrAuthors)
              p
              (book ^. gbrPublished)
              (book ^. gbrBooksLink)
              Map.empty
              (DSGoogle $ book ^. gbrCoverLink)

fetchImageFromUrl :: Ctx -> Text -> IO LBS.ByteString
fetchImageFromUrl ctx url = do
    request <- HTTP.parseRequest $ T.unpack url
    response <- HTTP.httpLbs request (ctxManager ctx)
    pure $ HTTP.responseBody response

-------------------------------------------------------------------------------
-- Implementations
-------------------------------------------------------------------------------

--Helper function to load local cover pictures to sisosota
_updateAllBookCovers :: Ctx -> Handler ()
_updateAllBookCovers ctx = do
    books <- runLogT "fetch-cover" (ctxLogger ctx) $ fetchCoverInformationsAsText (ctxPostgres ctx)
    for_ books fetchAndSendCover
  where
      fetchAndSendCover :: (BookInformationId, Text) -> Handler ()
      fetchAndSendCover (binfoId, coverText) = do
          pictData <- if all isSpace (T.unpack coverText) || not (T.isPrefixOf "cover" coverText) then
                        pure Nothing
                      else
                        Just <$> liftIO (LBS.readFile (T.unpack $ "ADD LOCAL MEDIA DIR" <> coverText))
          contentHash <- case pictData of
                           Nothing -> pure Nothing
                           Just pic -> Just <$> liftIO (addNewCover ctx pic)
          _ <- case contentHash of
                 Just h -> runLogT "update-cover" (ctxLogger ctx) $ updateBookCover (ctxPostgres ctx) binfoId h
                 Nothing -> pure 0
          pure ()

getBookCoverImpl :: Ctx -> ContentHash -> Handler (Headers '[Header "Cache-Control" Text] DynamicImage)
getBookCoverImpl ctx picture = do
    picData <- fetchCover ctx picture
    case picData of
      Right pic -> case decodeImage $ LBS.toStrict pic of
        Left _err -> throwError $ err404 { errBody = "Decoding failed" }
        Right p -> pure $ addHeader "public, max-age=31536000" p
      Left _err -> throwError $ err404 { errBody = "Fetching cover failed" }

indexPageImpl :: Ctx
              -> Maybe (Some SortCriteria)
              -> Maybe SortDirection
              -> Maybe Int
              -> Maybe BookInformationId
              -> Maybe BoardGameInformationId
              -> Maybe Text
              -> Maybe LibraryOrAll
              -> Maybe Text
              -> Handler (HtmlPage "indexpage")
indexPageImpl ctx mcriteria direction limit startBookId startBoardGameId search library onlyAvailableText =
    case criteria of
        MkSome crit@(BookSort _)      -> do
            mii <- maybe (pure Nothing) (runLogT "fetch-information" (ctxLogger ctx) . fmap2 ItemBook . fetchBookInformation ctx) startBookId
            go crit mii
        MkSome crit@(BoardGameSort _) -> do
            mii <- maybe (pure Nothing) (runLogT "fetch-information" (ctxLogger ctx) . fmap2 ItemBoardGame . fetchBoardGameInformation ctx) startBoardGameId
            go crit mii
  where
    -- the rest of the block is in separate function, as we want
    -- this to be in the `case ... of`, where we now the value of `ty`
    go :: SItemTypeI ty => SortCriteria ty -> Maybe (ItemInfo ty) ->  Handler (HtmlPage "indexpage")
    go crit mii = do
        -- items to show
        itemInfos <- runLogT "fetch-information-with-criteria" (ctxLogger ctx) $ fetchInformationsWithCriteria ctx crit mii dir lim cleanedSearch lib onlyAvailable
        pure $ indexPage crit itemInfos dir lim startBookId startBoardGameId search library onlyAvailableText

    fmap2 f = fmap (fmap f)

    -- default values
    criteria      = fromMaybe (MkSome (BookSort SortTitle)) mcriteria
    lib           = fromMaybe AllLibraries library
    dir           = fromMaybe SortAsc direction
    lim           = fromMaybe 10 limit
    onlyAvailable = isJust onlyAvailableText
    cleanedSearch = search >>= (\s -> if s == "" then Nothing else Just s)

bookInformationPageImpl :: Ctx -> BookInformationId -> Handler (HtmlPage "bookinformation")
bookInformationPageImpl ctx binfoid = do
    book <- getBookImpl ctx binfoid
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    ls <- runLogT "fetch-loans" (ctxLogger ctx) $ fetchLoansWithItemIds ctx (_booksBookId <$> _books book)
    pure $ bookInformationPage book ls es

boardGameInformationPageImpl :: Ctx -> BoardGameInformationId -> Handler (HtmlPage "boardgameinformation")
boardGameInformationPageImpl ctx boardgameInfoId = do
    boardgameResponse <- runLogT "fetch-boardgame-response" (ctxLogger ctx) $ fetchBoardGameResponse ctx boardgameInfoId
    case boardgameResponse of
      Just response -> do
          es <- liftIO $ readTVarIO $ ctxPersonio ctx
          ls <- runLogT "fetch-loans" (ctxLogger ctx) $ fetchLoansWithItemIds ctx (_boardGamesBoardGameId <$> response ^. boardGameResponseGames)
          pure $ boardGameInformationPage response ls es
      Nothing -> throwError err404

personalLoansPageImpl :: Ctx -> Maybe Login -> Handler (HtmlPage "personalinformation")
personalLoansPageImpl ctx login = do
    ls <- personalLoansImpl ctx login
    pure $ personalLoansPage ls

borrowBookCallback :: Ctx -> BorrowRequest -> Login -> Handler Loan
borrowBookCallback ctx req l = do
    emap <- liftIO $ getPersonioDataMap ctx
    case emap ^.at l of
      Nothing -> throwError err403
      Just es -> do
          loanData <- runLogT "borrow-book" (ctxLogger ctx) (borrowBook ctx (es ^. P.employeeId) req)
          case loanData of
            Nothing -> throwError $ err404 { errBody = "Loan data is not available" }
            Just (LoanData lid _ _ _) -> getLoanImpl ctx lid

borrowBookImpl :: Ctx -> Maybe Login -> BorrowRequest -> Handler Loan
borrowBookImpl ctx login req = withAuthUser ctx login (borrowBookCallback ctx req)

borrowWithUserPostImpl :: Ctx -> BorrowRequestWithUser -> Handler LoanResponse
borrowWithUserPostImpl ctx (BorrowRequestWithUser user req) = do
    (Loan lid date info person) <- borrowBookCallback ctx req user
    return $ LoanResponse lid date (info ^. itemInfo) ((\p -> (p ^. P.employeeFirst) <> " " <> (p ^. P.employeeLast)) <$> person)

snatchBookImpl :: Ctx -> Maybe Login -> ItemId -> Handler Loan
snatchBookImpl ctx login iid = withAuthUser ctx login $ (\l -> do
    emap <- liftIO $ getPersonioDataMap ctx
    newLoan <- runLogT "snatch" (ctxLogger ctx) $ runMaybeT $ do
        loanData <- MaybeT $ fetchLoanIdWithItemId ctx iid
        _ <- returnItem ctx (ldLoanId loanData)
        itemData <- MaybeT $ fetchItem ctx iid
        es <- MaybeT $ pure $ emap ^.at l
        newLoanData <- case itemData of
           ItemData _ library (BookInfoId binfoid) -> MaybeT $ borrowBook ctx (es ^. P.employeeId) (BorrowRequest binfoid library)
           _ -> throwError err403
        pure $ getLoanImpl ctx (ldLoanId newLoanData)
    fromMaybe (throwError err403 { errBody = "Couldn't make forced loan"}) newLoan)

returnLoanImpl :: Ctx -> LoanId -> Handler Bool
returnLoanImpl ctx lid = runLogT "return-loan" (ctxLogger ctx) $ returnItem ctx lid

getBooksImpl :: Ctx -> Handler [BookInformationResponse]
getBooksImpl ctx = do
    bookInfos <- runLogT "fetch-book" (ctxLogger ctx) $ fetchBookInformations ctx
    runLogT "fetch-book-response" (ctxLogger ctx) $ fetchBooksResponse ctx bookInfos

getBookImpl :: Ctx -> BookInformationId -> Handler BookInformationResponse
getBookImpl ctx lid = do
    infos <- runLogT "fetch-book-response" (ctxLogger ctx) $ fetchBookResponse ctx lid
    case listToMaybe infos of
      Just info -> pure info
      Nothing -> throwError $ err404 { errBody = "No bookinformation found"}

getBookByISBNImpl :: Ctx -> Text -> Handler BookInformationByISBNResponse
getBookByISBNImpl ctx isbn = do
    info <- runLogT "fetch-by-isbn" (ctxLogger ctx) $ fetchBookInformationByISBN ctx cleanedISBN
    googleInfo <- liftIO $ fetchBookInformationFromGoogle ctx cleanedISBN
    case (bookInformationToISBNresponse <$> info) <|> googleInfo of
      Just i -> pure i
      Nothing -> throwError $ err404 { errBody = "No book with that ISBN found" }
  where
      booksPerLibrary x = Map.fromListWith (+) ((\(Books lib _) -> (lib,1)) <$> x)
      bookInformationToISBNresponse BookInformationResponse{..} = BookInformationByISBNResponse
          { _byISBNTitle      = _title
          , _byISBNISBN       = _ISBN
          , _byISBNAuthor     = _author
          , _byISBNPublisher  = _publisher
          , _byISBNPublished  = _published
          , _byISBNInfoLink   = _infoLink
          , _byISBNBooks      = booksPerLibrary _books
          , _byISBNDataSource = DSDatabase _id _cover}
      cleanedISBN = T.filter (/= '-') isbn

getLoansImpl :: Ctx -> Handler [LoanResponse]
getLoansImpl ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    ls <- runLogT "fetch-loans" (ctxLogger ctx) $ fetchLoans ctx es
    pure $ toLoanResponse <$> ls
  where
    toLoanResponse (Loan lid date info person) = LoanResponse lid date (info ^. itemInfo) ((\p -> (p ^. P.employeeFirst) <> " " <> (p ^. P.employeeLast)) <$> person)

getLoanImpl :: Ctx -> LoanId -> Handler Loan
getLoanImpl ctx lid = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    loanInfo <- runLogT "fetch-loan" (ctxLogger ctx) $ fetchLoan ctx es lid
    case loanInfo of
      Just loan' -> pure loan'
      Nothing -> throwError $ err404 { errBody = "No loan information found"}

personalLoansImpl :: Ctx -> Maybe Login -> Handler [Loan]
personalLoansImpl ctx login = withAuthUser ctx login $ (\l -> do
    emap <- liftIO $ getPersonioDataMap ctx
    eidmap <- liftIO $ readTVarIO $ ctxPersonio ctx
    case emap ^.at l of
      Nothing -> throwError err403
      Just es -> runLogT "fetch-personal-loans" (ctxLogger ctx) $ fetchPersonalLoans ctx eidmap (es ^. P.employeeId))

addBookPostImpl :: Ctx -> AddBookInformation -> Handler (HtmlPage "additempage")
addBookPostImpl ctx addBook@AddBookInformation{..} = do
    existingBook <- runLogT "check-existing-book" (ctxLogger ctx) $ checkIfExistingBook ctx _addBookInformationId _addBookISBN
    contentHash <- case existingBook of
      Nothing -> liftIO $ Just <$> case _addBookCover of
        CoverData coverData -> addNewCover ctx (fdPayload coverData)
        CoverUrl url -> fetchImageFromUrl ctx url >>= addNewCover ctx
      Just _binfoid -> pure Nothing
    res <- runLogT "add-book" (ctxLogger ctx) $ addNewBook ctx addBook contentHash
    if res then
      addItemPageGetImpl ctx
    else
      throwError err400

addBoardGamePostImpl :: Ctx -> AddBoardGameInformation -> Handler (HtmlPage "additempage")
addBoardGamePostImpl ctx addBoardGame = do
    res <- runLogT "add-boardgame" (ctxLogger ctx) $ addNewBoardGame ctx addBoardGame
    if res then
      addItemPageGetImpl ctx
    else
      throwError err400

editBookInformationPageImpl :: Ctx -> BookInformationId -> Handler (HtmlPage "edititempage")
editBookInformationPageImpl ctx infoId = runLogT "edit-bookinformation" (ctxLogger ctx) $ do
    info <- fetchBookInformation ctx infoId
    books <- fetchItemsByBookInformation ctx infoId
    b <- for books $ \(ItemData itemid library _) -> do
        status <- checkLoanStatus ctx itemid
        pure (itemid, status, library)
    case info of
      Just i -> pure $ editItemPage (Left (i, b))
      Nothing -> throwError err400

editBoardGameInformationPageImpl :: Ctx -> BoardGameInformationId -> Handler (HtmlPage "edititempage")
editBoardGameInformationPageImpl ctx infoId = runLogT "edit-boardgameinformation" (ctxLogger ctx) $ do
    info <- fetchBoardGameInformation ctx infoId
    boardgames <- fetchItemsByBoardGameInformationId ctx infoId
    b <- for boardgames $ \(ItemData itemid library _) -> do
        status <- checkLoanStatus ctx itemid
        pure (itemid, status, library)
    case info of
      Just i -> pure $ editItemPage (Right (i, b))
      Nothing -> throwError err400

editBookInformationImpl :: Ctx -> EditBookInformation -> Handler (HtmlPage "bookinformation")
editBookInformationImpl ctx info = do
    _ <- runLogT "update-book-information" (ctxLogger ctx) $ updateBookInformation ctx info
    bookInformationPageImpl ctx (info ^. editBookInformationId)

editBoardGameInformationImpl :: Ctx -> EditBoardGameInformation -> Handler (HtmlPage "boardgameinformation")
editBoardGameInformationImpl ctx info = do
    _ <- runLogT "update-boardgame-information" (ctxLogger ctx) $ updateBoardGameInformation ctx info
    boardGameInformationPageImpl ctx (info ^. editBoardGameInformationId)

addItemPageGetImpl :: Ctx -> Handler (HtmlPage "additempage")
addItemPageGetImpl _ = pure addItemPage

itemDeleteImpl :: Ctx -> ItemId -> Handler Text
itemDeleteImpl ctx itemid = runLogT "delete-item" (ctxLogger ctx) $ do
    loanData <- fetchLoanByItemId ctx itemid --don't delete item that is loaned
    case loanData of
      Just _ -> throwError err405 { errBody = "Item is loaned"}
      Nothing -> do
          itemData <- fetchItem ctx itemid
          res <- deleteItem ctx itemid
          if res then
            case itemData of
              Just (ItemData _ _ (BookInfoId infoid)) -> pure (linkToText $ fieldLink editBookPageGet infoid)
              Just (ItemData _ _ (BoardGameInfoId infoid)) -> pure (linkToText $ fieldLink editBoardGamePageGet infoid)
              Nothing -> throwError err500 { errBody = "No item information found" }
          else
            throwError err405 { errBody = "Item is loaned"}

addItemPostImpl :: Ctx -> AddItemRequest -> Handler (HtmlPage "edititempage")
addItemPostImpl ctx req = do
    res <- runLogT "add-item" (ctxLogger ctx) $ addItem ctx req
    case (res, req) of
      (True, AddItemRequest _ (Just bookinfoid) _) -> editBookInformationPageImpl ctx bookinfoid
      (True, AddItemRequest _ _ (Just boardgameinformationId)) -> editBoardGameInformationPageImpl ctx boardgameinformationId
      _ -> throwError err404

sendReminderEmailsImpl :: Ctx -> Maybe Login -> Handler Bool
sendReminderEmailsImpl ctx login = withAuthUser' ctx login $ \_ -> do
    emps <- liftIO $ readTVarIO $ ctxPersonio ctx
    _ <- runLogT "loan-remainder" (ctxLogger ctx) $ sendReminderEmails ctx emps
    return True

withAuthUser :: Ctx -> Maybe Login -> (Login -> Handler a) -> Handler a
withAuthUser ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing    -> throwError err403
    Just login -> f login
  where
    cfg = ctxConfig ctx

withAuthUser'
    :: Ctx
    -> Maybe Login
    -> (Login -> Handler a)
    -> Handler a
withAuthUser' ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing -> throwError err403
    Just login -> do
        now <- currentTime
        notMember <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            pure $ login `Set.notMember` fus
        if notMember then throwError err403 else f login
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
