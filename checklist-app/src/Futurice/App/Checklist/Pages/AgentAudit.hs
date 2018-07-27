{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.AgentAudit (agentAuditPage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

agentAuditPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> FUM.Login
    -> [(Command Identity, UTCTime)]
    -> HtmlPage "agent-audit"
agentAuditPage _world authUser agent cmds =
    checklistPage_ (FUM.loginToText agent <> "'s audit log") [] authUser (Just NavMore) $
        table_ $ do
            tr_ $ do
                th_ "When"
                th_ "What"
            for_ cmds $ \(cmd, stamp) -> tr_ $ do
                td_ [ style_ "white-space: nowrap" ] $ toHtml $ formatHumanHelsinkiTime stamp
                td_ $ toHtml $ textShow cmd
