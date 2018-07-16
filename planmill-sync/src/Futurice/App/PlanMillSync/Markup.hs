{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.PlanMillSync.Markup (
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
    | NavOnlyInPlanmill
    | NavOnlyInPersonio
    | NavCrossEmployees
    | NavCrossSubcontractors
    | NavCrossInactive
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "PlanMill Sync"

    navLink NavHome                = (href_ "/", "PlanMill Sync")
    navLink NavOnlyInPlanmill      = (href_ "/#planmill", "Only in PlanMill")
    navLink NavOnlyInPersonio      = (href_ "/#personio", "Only in Personio")
    navLink NavCrossEmployees      = (href_ "/#cross-employees", "Crosscheck: Employees")
    navLink NavCrossSubcontractors = (href_ "/#cross-subcontractors", "Crosscheck: Subcontractors")
    navLink NavCrossInactive       = (href_ "/#cross-inactive", "Crosscheck: Inactive")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "planmill-sync.js" >>= embedJS)
