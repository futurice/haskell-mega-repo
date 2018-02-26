{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.GitHubSync.AuditPage (auditPage) where

import Control.Lens              (contains, filtered)
import Data.Map.Lens             (toMapOf)
import Data.Set.Lens             (setOf)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.GitHubSync.Config (Pinned (..))

import qualified GitHub   as GH
import qualified Personio as P

auditPage
    :: [()]
    -> HtmlPage "audit"
auditPage _ = page_ "GitHub sync -  Audit" $ do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ "GitHub sync"

            li_ $ a_ [ href_ "/" ] "Report"
            li_ $ a_ [ href_ "/audit" ] "Audit log"

    fullRow_ $ h1_ "Audit log"
    fullRow_ "Nothing to see here yet"
