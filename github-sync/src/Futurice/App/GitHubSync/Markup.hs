{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.GitHubSync.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation
import Futurice.Prelude
import Prelude ()

data Nav
    = NavHome
    | NavAuditLog
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "GitHub Sync"

    navLink NavHome     = (href_ "/", "GitHub Sync")
    navLink NavAuditLog = (href_ "/audit", "Audit log")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "github-sync.js" >>= embedJS)


