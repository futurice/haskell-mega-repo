{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Badge.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_,)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Badge.API

data Nav
    = NavHome
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _  = "Badge"
    navLink NavHome = (recordHref_ recIndex Nothing, "Badge")
