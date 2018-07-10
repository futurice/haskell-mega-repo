{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Markup where

import Futurice.Generics
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant                   (Link)

data Nav =
    NavHome
    deriving (Eq, Enum, Bounded)

pageParams :: PageParams
pageParams = defPageParams

navLink :: Nav -> (Attribute, Text)
navLink NavHome = (href_ "/", "Home")

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

navigation_ :: Monad m => Maybe Nav -> HtmlT m ()
navigation_ nav' = div_ [ class_ "top-bar" ] $ fullRow_ $
    div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "menu horizontal" ] $ do
        for_ [minBound .. maxBound] $ \nav -> do
            let (aAttr, t) = navLink nav
            let liAttrs = if Just nav == nav' then [ class_ "futu-active" ] else []
            li_ liAttrs $ a_ [ aAttr ] $ toHtml t
