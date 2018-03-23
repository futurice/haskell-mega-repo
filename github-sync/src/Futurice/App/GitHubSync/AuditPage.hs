{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.GitHubSync.AuditPage (auditPage) where

import Futurice.Prelude
import Prelude ()

import Futurice.App.GitHubSync.Markup

import qualified FUM.Types.Login as FUM

auditPage
    :: [(FUM.Login, UTCTime, Text)]
    -> HtmlPage "audit"
auditPage xs = page_ "Audit" (Just NavAuditLog) $ do
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Who"
            th_ "When"
            th_ "What"

        tbody_ $ for_ xs $ \(login, stamp, command) -> tr_ $ do
            td_ $ toHtml login
            td_ $ toHtml $ formatHumanHelsinkiTime stamp
            td_ $ pre_ $ toHtml command
