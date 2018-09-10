{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.Library (defaultMain) where

import Codec.Picture                (DynamicImage, decodeImage)
import Crypto.Hash                  (HMAC (..), SHA256, hmac)
import Data.Byteable                (toBytes)
import Data.Char                    (isSpace)
import Data.Time
       (defaultTimeLocale, formatTime, getCurrentTime, iso8601DateFormat)
import FUM.Types.Login
import Futurice.App.Sisosota.Client
import Futurice.App.Sisosota.Types  (ContentHash)
import Futurice.Constants           (servicePublicUrl)
import Futurice.IdMap               (IdMap, fromFoldable)
import Futurice.Integrations
import Futurice.Lucid.Foundation    (HtmlPage)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Types.URI       (urlEncode)
import Prelude ()
import Servant
import Servant.Client
import Servant.Multipart
import Servant.Server.Generic
import Text.Read                    (readMaybe)

import Futurice.App.Library.API
import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.Logic
import Futurice.App.Library.Pages.AddItemPage
import Futurice.App.Library.Pages.BookInformationPage
import Futurice.App.Library.Pages.IndexPage
import Futurice.App.Library.Pages.PersonalLoansPage
import Futurice.App.Library.Types

import qualified Data.ByteString        as DB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as DBL
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Network.HTTP.Client    as N
import qualified Personio               as P
import qualified Xeno.DOM               as X

apiServer :: Ctx -> Server LibraryAPI
apiServer ctx = genericServer $ Record
    { booksGet             = getBooksImpl ctx
    , bookGet              = getBookImpl ctx
    , bookByISBNGet        = getBookByISBNImpl ctx
    , bookCoverGet         = getBookCoverImpl ctx
    , borrowPost           = borrowBookImpl ctx
    , snatchPost           = snatchBookImpl ctx
    , loansGet             = getLoansImpl ctx
    , loanGet              = getLoanImpl ctx
    , returnPost           = returnLoanImpl ctx
    , personalLoansGet     = personalLoansImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { addBookPost          = addBookPostImpl ctx
    , addBoardGamePost     = addBoardGamePostImpl ctx
    , addItemPageGet       = addItemPageGetImpl ctx
    , bookPageGet          = bookInformationPageImpl ctx
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
makeCtx cfg lgr mgr cache _mq = do
    pp <- createPostgresPool $ cfgPostgresConnInfo cfg
    return (Ctx cfg pp lgr mgr cache, [])

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[Proxy, Proxy, Proxy, Proxy, Proxy, I] a -> IO a
runIntegrations' (Ctx cfg _ lgr mgr _cache) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

getPersonioData :: Ctx -> IO (IdMap P.Employee)
getPersonioData ctx = do
    es' <- cachedIO (ctxLogger ctx) (ctxCache ctx) 180 () $ runIntegrations' ctx P.personioEmployees
    pure $ fromFoldable es'

getPersonioDataMap :: Ctx -> IO (Map Login P.Employee)
getPersonioDataMap ctx = do
    es' <- cachedIO (ctxLogger ctx) (ctxCache ctx) 180 () $ runIntegrations' ctx P.personioEmployees
    pure $ Map.fromList $ catMaybes $ fmap (\e -> case e ^. P.employeeLogin of
                                               Just login -> Just (login, e)
                                               Nothing -> Nothing) es'

fetchCover :: (Monad m, MonadIO m, MonadThrow m) => Ctx -> ContentHash -> m (Either String DBL.ByteString)
fetchCover ctx contentHash = do
    url <- parseBaseUrl $ T.unpack $ servicePublicUrl SisosotaService
    liftIO $ sisosotaGet (ctxManager ctx) url contentHash

addNewCover :: Ctx -> DBL.ByteString -> IO ContentHash
addNewCover ctx cover = do
    url <- parseBaseUrl $ T.unpack $ servicePublicUrl SisosotaService
    sisosotaPut (ctxManager ctx) url cover

makeAmazonAddress :: Ctx -> Text -> IO ByteString
makeAmazonAddress ctx isbn = do
    timeStamp <- formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%EZ")) <$> getCurrentTime
    pure $ "https://webservices.amazon.com/onca/xml?"
        <> queryParameters timeStamp
        <> "&Signature="
        <> (urlEncode True . B64.encode . toBytes . hmacGetDigest . calcSign $ baseAddress timeStamp)
  where
    queryParameters ts = "AWSAccessKeyId=" <> TE.encodeUtf8 (cfgAmazonAccessKey cfg) <> "&"
        <> "AssociateTag=" <> TE.encodeUtf8 (cfgAmazonAssociateTag cfg) <>"&"
        <> "IdType=ISBN&"
        <> "ItemId=" <> TE.encodeUtf8 isbn <> "&"
        <> "Operation=ItemLookup&"
        <> "ResponseGroup=Medium&"
        <> "SearchIndex=Books&"
        <> "Service=AWSECommerceService&"
        <> "Timestamp=" <> TE.encodeUtf8 (T.replace ":" "%3A" $ T.pack ts)
    baseAddress ts = "GET\n"
        <> "webservices.amazon.com\n"
        <> "/onca/xml\n"
        <> queryParameters ts
    calcSign :: ByteString -> HMAC SHA256
    calcSign = hmac (TE.encodeUtf8 $ cfgAmazonSecretKey cfg)
    cfg = ctxConfig ctx

fetchBookInformationFromAmazon :: Ctx -> Text -> IO (Maybe BookInformationByISBNResponse)
fetchBookInformationFromAmazon ctx isbn = do
    amazonAddress <- makeAmazonAddress ctx isbn
    request <- N.parseRequest $ C.unpack amazonAddress
    response <- N.httpLbs request (ctxManager ctx)
    case X.parse $ DBL.toStrict $ N.responseBody response of
      Left _error -> pure Nothing
      Right ns -> pure $ do
          items <- (findItems . X.children) ns
          item <- (findItem . X.children) items
          itemAttributes <- (findItemAttributes . X.children) item
          detailPageUrl <- (findDetailPageUrl . X.children) item
          image <- (findImage . X.children) item
          imageUrl <- (findImageUrl . X.children) image
          BookInformationByISBNResponse
              <$> (TE.decodeUtf8 <$> (getTitle . X.children) itemAttributes)
              <*> Just isbn
              <*> (TE.decodeUtf8 <$> (getAuthors . X.children) itemAttributes)
              <*> (TE.decodeUtf8 <$> (getPublisher . X.children) itemAttributes)
              <*> (((take 4 . T.unpack . TE.decodeUtf8) <$> (getPublished . X.children) itemAttributes) >>= readMaybe)
              <*> (TE.decodeUtf8 <$> getUrlLink detailPageUrl)
              <*> Just Map.empty
              <*> (DSAmazon . TE.decodeUtf8 <$> getUrlLink imageUrl)
  where
      findItems = (listToMaybe . filter (\n -> X.name n == "Items"))
      findItem = (listToMaybe . filter (\n -> X.name n == "Item"))
      findItemAttributes = (listToMaybe . filter (\n -> X.name n == "ItemAttributes"))
      findDetailPageUrl = (listToMaybe . filter (\n -> X.name n == "DetailPageURL"))
      findImage = (listToMaybe . filter (\n -> X.name n == "LargeImage"))
      findImageUrl = (listToMaybe . filter (\n -> X.name n == "URL"))
      getUrlLink urlNode = (listToMaybe $ X.contents urlNode) >>= contentText
      getAuthors attrs = do
          authors <- traverse contentText (concat (X.contents <$> (filter (\n -> X.name n == "Author") attrs)))
          pure $ DB.intercalate " and " authors
      getValue val attrs =  do
          content <- X.contents <$> (listToMaybe . filter (\n -> X.name n == val)) attrs
          firstElement <- listToMaybe content
          contentText firstElement
      getTitle = getValue "Title"
      getPublisher = getValue "Publisher"
      getPublished = getValue "PublicationDate"
      contentText (X.Text text) = Just text
      contentText _             = Nothing

fetchImageFromAmazon :: Ctx -> Text -> IO DBL.ByteString
fetchImageFromAmazon ctx url = do
    request <- N.parseRequest $ T.unpack url
    response <- N.httpLbs request (ctxManager ctx)
    pure $ N.responseBody response

-------------------------------------------------------------------------------
-- Implementations
-------------------------------------------------------------------------------

--Helper function to load local cover pictures to sisosota
_updateAllBookCovers :: Ctx -> Handler ()
_updateAllBookCovers ctx = do
    books <- runLogT "library" (ctxLogger ctx) $ fetchCoverInformationsAsText ctx
    for_ books fetchAndSendCover
  where
      fetchAndSendCover :: (BookInformationId, Text) -> Handler ()
      fetchAndSendCover (binfoId, coverText) = do
          pictData <- if all isSpace (T.unpack coverText) || not (T.isPrefixOf "cover" coverText) then
                        pure Nothing
                      else
                        Just <$> liftIO (DBL.readFile (T.unpack $ "ADD LOCAL MEDIA DIR" <> coverText))
          contentHash <- case pictData of
                           Nothing -> pure Nothing
                           Just pic -> Just <$> liftIO (addNewCover ctx pic)
          _ <- case contentHash of
                 Just h -> runLogT "library" (ctxLogger ctx) $ updateBookCover ctx binfoId h
                 Nothing -> pure 0
          pure ()

getBookCoverImpl :: Ctx -> ContentHash -> Handler (Headers '[Header "Cache-Control" Text] (DynamicImage))
getBookCoverImpl ctx picture = do
    picData <- fetchCover ctx picture
    case picData of
      Right pic -> case decodeImage $ DBL.toStrict pic of
        Left _err -> throwError $ err404 { errBody = "Decoding failed" }
        Right p -> pure $ addHeader "public, max-age=3600" p
      Left _err -> throwError $ err404 { errBody = "Fetching cover failed" }

indexPageImpl :: Ctx
              -> Maybe SortCriteria
              -> Maybe SortDirection
              -> Maybe Int
              -> Maybe BookInformationId
              -> Maybe Text
              -> Handler (HtmlPage "indexpage")
indexPageImpl ctx criteria direction limit startId search = do
    crit <- pure $ fromMaybe SortTitle criteria
    dir <- pure $ fromMaybe SortAsc direction
    lim <- pure $ fromMaybe 10 limit
    bookInfos <- runLogT "library" (ctxLogger ctx) $ fetchBookInformationsWithCriteria ctx crit dir lim startId search
    books <- runLogT "library" (ctxLogger ctx) $ fetchBooksResponse ctx bookInfos
    pure $ indexPage books crit dir lim startId search

bookInformationPageImpl :: Ctx -> BookInformationId -> Handler (HtmlPage "bookinformation")
bookInformationPageImpl ctx binfoid = do
    book <- getBookImpl ctx binfoid
    es <- liftIO $ getPersonioData ctx
    ls <- runLogT "library" (ctxLogger ctx) $ fetchLoansWithItemIds ctx (_booksBookId <$> _books book)
    pure $ bookInformationPage book ls es

personalLoansPageImpl :: Ctx -> Maybe Login -> Handler (HtmlPage "personalinformation")
personalLoansPageImpl ctx login = do
    ls <- personalLoansImpl ctx login
    pure $ personalLoansPage ls

borrowBookImpl :: Ctx -> Maybe Login -> BorrowRequest -> Handler Loan
borrowBookImpl ctx login req = withAuthUser ctx login $ (\l -> do
    emap <- liftIO $ getPersonioDataMap ctx
    case emap ^.at l of
      Nothing -> throwError err403
      Just es -> do
          loanData <- runLogT "library" (ctxLogger ctx) (borrowBook ctx (es ^. P.employeeId) req)
          case loanData of
            Nothing -> throwError $ err404 { errBody = "Loan data is not available" }
            Just (LoanData lid _ _ _) -> getLoanImpl ctx lid)

snatchBookImpl :: Ctx -> Maybe Login -> ItemId -> Handler Loan
snatchBookImpl ctx login iid = withAuthUser ctx login $ (\l -> do
    emap <- liftIO $ getPersonioDataMap ctx
    case emap ^.at l of
      Nothing -> throwError err403
      Just es -> do
          loanData <- runLogT "library" (ctxLogger ctx) $ snatchBook ctx (es ^. P.employeeId) iid
          case loanData of
            Nothing -> throwError $ err404 { errBody = "Couln't loan book" }
            Just (LoanData lid _ _ _) -> getLoanImpl ctx lid)

returnLoanImpl :: Ctx -> LoanId -> Handler Bool
returnLoanImpl ctx lid = runLogT "library" (ctxLogger ctx) (executeReturnBook ctx lid)

getBooksImpl :: Ctx -> Handler [BookInformationResponse]
getBooksImpl ctx = do
    bookInfos <- runLogT "library" (ctxLogger ctx) $ fetchBookInformations ctx
    runLogT "library" (ctxLogger ctx) $ fetchBooksResponse ctx bookInfos

getBookImpl :: Ctx -> BookInformationId -> Handler BookInformationResponse
getBookImpl ctx lid = do
    infos <- runLogT "library" (ctxLogger ctx) $ fetchBookResponse ctx lid
    case listToMaybe infos of
      Just info -> pure info
      Nothing -> throwError $ err404 { errBody = "No bookinformation found"}

getBookByISBNImpl :: Ctx -> Text -> Handler BookInformationByISBNResponse
getBookByISBNImpl ctx isbn = do
    info <- runLogT "library" (ctxLogger ctx) $ fetchBookInformationByISBN ctx cleanedISBN
    amazonInfo <- liftIO $ fetchBookInformationFromAmazon ctx cleanedISBN
    case (bookInformationToISBNresponse <$> info) <|> amazonInfo of
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
          , _byISBNAmazonLink = _amazonLink
          , _byISBNBooks      = booksPerLibrary _books
          , _byISBNDataSource = DSDatabase _id _cover}
      cleanedISBN = T.filter (/= '-') isbn

getLoansImpl :: Ctx -> Handler [Loan]
getLoansImpl ctx = do
    runLogT "library" (ctxLogger ctx) $ do
        es <- liftIO $ getPersonioData ctx
        fetchLoans ctx es

getLoanImpl :: Ctx -> LoanId -> Handler Loan
getLoanImpl ctx lid = do
    loanInfo <- runLogT "library" (ctxLogger ctx) $ do
        es <- liftIO $ getPersonioData ctx
        fetchLoan ctx es lid
    case loanInfo of
      Just loan' -> pure loan'
      Nothing -> throwError $ err404 { errBody = "No loan information found"}

personalLoansImpl :: Ctx -> Maybe Login -> Handler [Loan]
personalLoansImpl ctx login = withAuthUser ctx login $ (\l -> do
    emap <- liftIO $ getPersonioDataMap ctx
    eidmap <- liftIO $ getPersonioData ctx
    case emap ^.at l of
      Nothing -> throwError err403
      Just es -> runLogT "library" (ctxLogger ctx) $ fetchPersonalLoans ctx eidmap (es ^. P.employeeId))

addBookPostImpl :: Ctx -> AddBookInformation -> Handler (HtmlPage "additempage")
addBookPostImpl ctx addBook@AddBookInformation{..} = do
    existingBook <- runLogT "library" (ctxLogger ctx) $ checkIfExistingBook ctx _addBookInformationId _addBookISBN
    contentHash <- case existingBook of
      Nothing -> liftIO $ Just <$> case _addBookCover of
        CoverData coverData -> addNewCover ctx (fdPayload coverData)
        CoverUrl url -> fetchImageFromAmazon ctx url >>= addNewCover ctx
      Just _binfoid -> pure Nothing
    res <- runLogT "library" (ctxLogger ctx) $ addNewBook ctx addBook contentHash
    if res then
      addItemPageGetImpl ctx
    else
      throwError err400

addBoardGamePostImpl :: Ctx -> AddBoardGameInformation ->  Handler (HtmlPage "additempage")
addBoardGamePostImpl ctx addBoardGame = do
    res <- runLogT "library" (ctxLogger ctx) $ addNewBoardGame ctx addBoardGame
    if res then
      addItemPageGetImpl ctx
    else
      throwError err400

addItemPageGetImpl :: Ctx -> Handler (HtmlPage "additempage")
addItemPageGetImpl _ = pure addItemPage

withAuthUser :: Ctx -> Maybe Login -> (Login -> Handler a) -> Handler a
withAuthUser ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing    -> throwError err403
    Just login -> f login
  where
    cfg = ctxConfig ctx
