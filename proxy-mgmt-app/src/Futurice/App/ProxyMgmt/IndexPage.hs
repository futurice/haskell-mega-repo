{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.IndexPage (indexPageHandler) where

import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Constants         (supportEmailHtml)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (cachedIO)
import Prelude ()

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types

indexPageHandler :: Ctx f -> ReaderT Login IO (HtmlPage "index")
indexPageHandler ctx = ReaderT $ \login -> do
    mtoken <- fetchToken login ctx
    entries <- fetchAccessEntries login ctx
    return $ indexPage login mtoken entries

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

fetchToken :: Login -> Ctx f -> IO (Maybe Token)
fetchToken login Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchTokens" ctxLogger $
        listToMaybe <$> safePoolQuery ctxPostgresPool
            "SELECT username, passtext is not null, usertype, endpoint FROM proxyapp.credentials WHERE username = ?;"
            (Only login)

fetchAccessEntries :: Login -> Ctx f -> IO [AccessEntry]
fetchAccessEntries login Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchAccessEntries" ctxLogger $ do
        safePoolQuery ctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE username = ? ORDER BY updated DESC LIMIT 100;"
            (Only login)

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

indexPage :: Login -> Maybe Token -> [AccessEntry] -> HtmlPage "index"
indexPage login mtoken entries =
    maybe (noTokenPage login) (tokenPage login entries) mtoken

noTokenPage :: Login -> HtmlPage "index"
noTokenPage login =
    page_ ("Prox management - " <> loginToText login <> " - no token") (Just NavIndex) $ do
        p_ $ do
            "You don't have active prox token. "
            "Contact IT at "
            supportEmailHtml
            ", if you need one."

        p_ $
            "What is prox? TBW"

tokenPage :: Login -> [AccessEntry] -> Token -> HtmlPage "index"
tokenPage login entries Token {..} = page_ ("Prox management - " <> loginToText login) (Just NavIndex) $ do
    h2_ "Token"
    condensedTable_ $ tbody_ $ do
        vertRow_ "Active"   $ if tActive then "Active" else "Passive"
        vertRow_ "Endpoint" $ toHtml tEndpoint

    h2_ "Regenerate token - TODO"
    button_ [ class_ "button" ] "Regenerate"

    h2_ "Last 100 accesses"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Timestamp"
            th_ "Endpoint"

        tbody_ $ for_ entries $ \AccessEntry {..} -> tr_ $ do
            td_ $ toHtml $ show aeStamp
            td_ $ toHtml aeEndpoint
