{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Preferences.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_, pageParamsWithJS)
import Futurice.Prelude
import Prelude ()

data Nav
    = NavHome
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Preferences"

    navLink NavHome                = (href_ "/", "Preferences")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "preferences-app.js" >>= embedJS)
