{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.Pages.Audit (auditPageHandler) where

import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import FUM.Types.Login
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant                   (cachedIO)
import Prelude ()

import Futurice.App.Proxy.API (Routes, proxyEndpoints)

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

auditPageHandler :: ReaderT (Login, Ctx f) IO (HtmlPage "audit")
auditPageHandler = do
    (_, ctx) <- ask
    liftIO $ do
        audit <- fetchAudit ctx
        pure $ auditPage audit

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

fetchAudit :: Ctx f -> IO [(Login, UTCTime, Text)]
fetchAudit Ctx {..} =
    runLogT "fetchAudit" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, created, message FROM proxyapp.auditlog ORDER BY created DESC;"

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

auditPage :: [(Login, UTCTime, Text)] -> HtmlPage "audit"
auditPage audit = page_ "Audit log" (Just NavAudit) $ do
    sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Username"
            th_ "Timestamp"
            th_ "Message"
        tbody_ $ for_ audit $ \(login, stamp, msg) -> tr_ $ do
            td_ $ toHtml login
            td_ $ toHtml . formatHumanHelsinkiTime $ stamp
            td_ $ toHtml msg
