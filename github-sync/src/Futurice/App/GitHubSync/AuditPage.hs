{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.GitHubSync.AuditPage (auditPage) where

import Control.Lens              (contains, filtered)
import Data.Map.Lens             (toMapOf)
import Data.Set.Lens             (setOf)
import Futurice.Prelude
import Prelude ()

import Futurice.App.GitHubSync.Config (Pinned (..))
import Futurice.App.GitHubSync.Markup

import qualified GitHub   as GH
import qualified Personio as P

auditPage
    :: [()]
    -> HtmlPage "audit"
auditPage _ = page_ "Audit" (Just NavAuditLog) $ do
    fullRow_ "Nothing to see here yet"
