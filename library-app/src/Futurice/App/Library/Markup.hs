{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Library.Markup (
    module Futurice.Lucid.Foundation,
    linkToText,
    Nav (..),
    page_) where

import Futurice.Generics
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithJS, page_)
import Futurice.Prelude
import Prelude ()
import Servant                   (Link)

import Futurice.App.Library.API

data Nav = NavHome
         | NavUser
    deriving (Eq, Enum, Bounded, Ord)

instance Navigation Nav where
    serviceTitle _ = "Library"

    navLink NavHome = (recordHref_ indexPageGet Nothing Nothing Nothing Nothing Nothing, "Library Home")
    navLink NavUser = (recordHref_ personalLoansPageGet, "My Loans")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "library-app.js" >>= embedJS)

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
