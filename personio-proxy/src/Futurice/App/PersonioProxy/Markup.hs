{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.PersonioProxy.Markup where

import Futurice.Prelude
import Prelude ()

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithJS)

data Nav = NavHome
         | NavStats
    deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Personio Proxy"

    navLink NavHome = (href_ "/", "Personio Proxy")
    navLink NavStats = (href_ "/stats", "Stats")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "personio-proxy.js" >>= embedJS)
