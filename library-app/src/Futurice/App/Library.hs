{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.Library (defaultMain) where

import Codec.Picture             (DynamicImage, readImage)
import Data.Text
import FUM.Types.Login
import Futurice.IdMap            (IdMap, fromFoldable)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Multipart
import Servant.Server.Generic

import Futurice.App.Library.API
import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.Logic
import Futurice.App.Library.Pages.AddItemPage
import Futurice.App.Library.Pages.BookInformationPage
import Futurice.App.Library.Pages.IndexPage
import Futurice.App.Library.Pages.PersonalLoansPage
import Futurice.App.Library.Types

import qualified Data.ByteString      as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Map             as Map
import qualified Personio             as P

apiServer :: Ctx -> Server LibraryAPI
apiServer ctx = genericServer $ Record
    { booksGet             = getBooksImpl ctx
    , bookGet              = getBookImpl ctx
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
-------------------------------------------------------------------------------
-- Implementations
-------------------------------------------------------------------------------

-- Helper function to migrate to using personio_id:s
-- migrateToPersonioNumberImpl :: Ctx -> Handler ()
-- migrateToPersonioNumberImpl ctx = do
--     emp <- liftIO $ getPersonioDataMap ctx
--     _ <- runLogT "library" (ctxLogger ctx) $ migrateToPersonioNumber ctx emp
--     pure ()

getBookCoverImpl :: Ctx -> Text -> Handler (Headers '[Header "Cache-Control" Text] (DynamicImage))
getBookCoverImpl _ctx picture = do
    pictData <- liftIO $ readImage (unpack $ "/Users/toku/hmr/library-app/media/" <> picture)
    case pictData of
      Left _ -> throwError $ err404 { errBody = "Cover not found" }
      Right pict -> pure $ addHeader "public, max-age=3600" pict

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
addBookPostImpl ctx addBook@(AddBookInformation _ _ _ _ _ _ _ coverData) = do
    liftIO $ DB.writeFile ("/Users/toku/hmr/library-app/media/" ++ unpack (fdFileName coverData)) $ DBL.toStrict $ fdPayload coverData
    res <- runLogT "library" (ctxLogger ctx) $ addNewBook ctx addBook
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
