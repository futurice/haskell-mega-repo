{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Markup (
    module Futurice.Lucid.Foundation,
    linkToText,
    Nav (..),
    page_) where

import Futurice.Generics
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), page_)
import Futurice.Prelude
import Prelude ()
import Servant                   (Link)

import Futurice.App.Library.API

data Nav =
    NavHome
    deriving (Eq, Enum, Bounded, Ord)

instance Navigation Nav where
    serviceTitle _ = "Library"

    navLink NavHome = (recordHref_ indexPageGet, "Library Home")

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
