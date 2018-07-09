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
import Futurice.Lucid.Foundation (HtmlPage, fullRow_, h1_, page_)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.IndexPage
import Futurice.App.Library.Logic
import Futurice.App.Library.Types

import qualified Data.Map as Map
import qualified Personio as P

server :: Ctx -> Server LibraryAPI
server ctx = indexPageImpl ctx
    :<|> getBooksImpl ctx
    :<|> getBookImpl ctx
    :<|> getBookCoverImpl ctx
    :<|> borrowBookImpl ctx
    :<|> snatchBookImpl ctx
    :<|> getLoansImpl ctx
    :<|> getLoanImpl ctx

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ LibraryService
    & serverDescription      .~ "Futushelf 2.0"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp libraryApi   .~ server
    & serverEnvPfx           .~ "LIBRARYAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache mq = do
    pp <- createPostgresPool $ cfgPostgresConnInfo cfg
    return (Ctx cfg pp lgr mgr cache, [])

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[Proxy, Proxy, Proxy, Proxy, Proxy, I] a -> IO a
runIntegrations' (Ctx cfg _ lgr mgr cache) m = do
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
getBookCoverImpl ctx picture = do
    pictData <- liftIO $ readImage (unpack $ "/Users/toku/hmr/library-app/media/" <> picture)
    case pictData of
      Left _ -> throwError $ err404 { errBody = "Cover not found" }
      Right pict -> pure $ addHeader "public, max-age=3600" pict

indexPageImpl :: Ctx -> Maybe Login -> Handler (HtmlPage "indexpage")
indexPageImpl ctx loc = withAuthUser ctx loc

borrowBookImpl :: Ctx -> Maybe Login -> BorrowRequest -> Handler Loan
borrowBookImpl ctx login req = throwError $ err500 { errBody = "Loan is not possible" }

snatchBookImpl :: Ctx -> Maybe Login -> BorrowRequest -> Handler Loan
snatchBookImpl ctx login req = throwError $ err404 { errBody = "Loan is not possible" }

getBooksImpl :: Ctx -> Handler [BookInformationResponse]
getBooksImpl ctx = runLogT "library" (ctxLogger ctx) $ fetchBooksResponse ctx

getBookImpl :: Ctx -> BookId -> Handler BookInformationResponse
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
        fetchLoan ctx lid es
    case loanInfo of
      Just loan' -> pure loan'
      Nothing -> throwError $ err404 { errBody = "No loan information found"}

withAuthUser :: Ctx -> Maybe Login -> Handler (HtmlPage "indexpage")
withAuthUser ctx loc = case loc <|> cfgMockUser cfg of
    Nothing -> pure page404
    Just fu -> pure page404
  where
    cfg = ctxConfig ctx

page404 :: HtmlPage a
page404 = page_ "Library - unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT for authorization"
