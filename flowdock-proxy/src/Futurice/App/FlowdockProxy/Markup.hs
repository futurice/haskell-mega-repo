{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_, pageParamsWithCSS)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FlowdockProxy.API
import Futurice.App.FlowdockProxy.Clay (css)

data Nav
    = NavHome
    | NavCharts
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Flowdock Proxy"

    navLink NavHome   = (recordHref_ recIndex Nothing Nothing Nothing, "Flowdock Proxy")
    navLink NavCharts = (recordHref_ recCharts, "Charts")

    pageParams = pageParamsWithCSS css
