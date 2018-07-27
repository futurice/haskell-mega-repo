{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Agents (agentsPage) where

import Control.Lens              (iforOf_)
import Data.Ord                  (Down (..))
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

agentsPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> [(FUM.Login, UTCTime)]
    -> HtmlPage "agents"
agentsPage _world authUser agents = checklistPage_ "Active agents" [] authUser (Just NavMore) $
    sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Agent"
            th_ "Last active"
            th_ mempty
        tbody_ $ iforOf_ (folded . ifolded) (sortOn (Down . snd) agents) $ \login stamp -> tr_ $ do
            td_ $ toHtml login
            td_ $ toHtml (formatHumanHelsinkiTime stamp)
            td_ $ a_ [ class_ "button", recordHref_ routeAgentAudit login ] "Audit"
