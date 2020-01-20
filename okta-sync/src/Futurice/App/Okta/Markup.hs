{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Okta.Markup(
    module Futurice.Lucid.Foundation,
    Nav (..),
    page_) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation
       (Navigation (..), futuriceCss, pageParamsWithJS, page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Okta.API

data Nav = NavHome deriving (Eq, Enum, Bounded, Ord)

instance Navigation Nav where
    serviceTitle _ = "Okta sync"

    navLink NavHome = (recordHref_ indexPageGet, "Okta sync home")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "okta-sync.js" >>= embedJS)
