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

data Nav
    = NavIndex
    | NavAdmin
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Prox management"

    navLink NavIndex = (href_ "/", "Prox management")
    navLink NavAdmin = (href_ "/admin", "Admin")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "proxy-mgmt.js" >>= embedJS)
