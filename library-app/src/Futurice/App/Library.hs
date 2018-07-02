{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.Library (defaultMain) where

import Codec.Picture             (DynamicImage, readImage)
import Data.Text
import FUM.Types.Login
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

import qualified Personio as P

server :: Ctx -> Server LibraryAPI
server ctx = indexPageImpl ctx
    :<|> getBooksImpl ctx
    :<|> getBookImpl ctx
    :<|> getBookCoverImpl ctx
    :<|> getLoansImpl ctx

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
    return (Ctx cfg pp lgr mgr, [])

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[Proxy, Proxy, Proxy, Proxy, Proxy, I] a -> IO a
runIntegrations' (Ctx cfg _ lgr mgr) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

-------------------------------------------------------------------------------
-- Implementations
-------------------------------------------------------------------------------

getBookCoverImpl :: Ctx -> Text -> Handler (Headers '[Header "Cache-Control" Text] (DynamicImage))
getBookCoverImpl ctx picture = do
    pictData <- liftIO $ readImage (unpack $ "/Users/toku/hmr/library-app/media/" <> picture)
    case pictData of
      Left _ -> throwError $ err404 { errBody = "Cover not found" }
      Right pict -> pure $ addHeader "public, max-age=3600" pict

indexPageImpl :: Ctx -> Maybe Login -> Handler (HtmlPage "indexpage")
indexPageImpl ctx loc = withAuthUser ctx loc

getBooksImpl :: Ctx -> Handler [BookInformationResponse]
getBooksImpl ctx = runLogT "library" (ctxLogger ctx) $ fetchBooks ctx

getBookImpl :: Ctx -> LoanableId -> Handler [BookInformationResponse]
getBookImpl ctx lid = runLogT "library" (ctxLogger ctx) $ fetchBook ctx lid

getLoansImpl :: Ctx -> Handler [Loan]
getLoansImpl ctx = do
    runLogT "library" (ctxLogger ctx) $ do
        es <- liftIO $ runIntegrations' ctx P.personioEmployees
        loans <- fetchLoans ctx es
        pure loans

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
