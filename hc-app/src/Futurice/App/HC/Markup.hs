{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_, pageParamsWithJS)
import Futurice.Prelude
import Prelude ()

import Futurice.App.HC.API

data Nav
    = NavHome
    | NavPersonioValidation
    | NavEarlyCaring
    | NavAchoo
    | NavAnniversaries
    | NavHrNumbers
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "HC"

    navLink NavHome               = (recordHref_ recIndex,               "HC Home")
    navLink NavPersonioValidation = (recordHref_ recPersonioValidations, "Personio data validations")
    navLink NavEarlyCaring        = (recordHref_ recEarlyCaring False,   "Early caring")
    navLink NavAchoo              = (recordHref_ recAchooReport Nothing Nothing Nothing, "Achoo report")
    navLink NavAnniversaries      = (recordHref_ recAnniversaries ,      "Anniversaries")
    navLink NavHrNumbers          = (recordHref_ recHrNumbers,           "HR Numbers")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "early-caring.js" >>= embedJS)
