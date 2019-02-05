{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.ProxyMgmt.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation
import Futurice.Prelude
import Prelude ()

import Futurice.App.ProxyMgmt.API

data Nav
    = NavIndex
    | NavTokens
    | NavPolicies
    | NavAudit
    | NavReports
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Prox management"

    navLink NavIndex    = (recordHref_ routeIndexPage,    "Prox management")
    navLink NavTokens   = (recordHref_ routeTokensPage,   "Tokens")
    navLink NavPolicies = (recordHref_ routePoliciesPage, "Policies")
    navLink NavAudit    = (recordHref_ routeAuditPage,    "Audit log")
    navLink NavReports  = (recordHref_ routeReportsPage,  "Reports")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "proxy-mgmt.js" >>= embedJS)
