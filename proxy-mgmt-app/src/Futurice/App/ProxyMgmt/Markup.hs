{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.ProxyMgmt.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), colourClay)
import Futurice.Prelude
import Prelude ()

import qualified Futurice.Lucid.Foundation as Lucid
import qualified Clay as C

data Nav
    = NavIndex
    | NavAdmin
  deriving (Eq, Enum, Bounded)

navLink :: Nav -> (Attribute, Text)
navLink NavIndex = (href_ "/", "Prox management")
navLink NavAdmin = (href_ "/admin", "Admin")

navigation_ :: Monad m => Maybe Nav -> HtmlT m ()
navigation_ nav' = div_ [ class_ "top-bar" ] $ fullRow_ $
    div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "menu horizontal" ] $ do
        for_ [minBound .. maxBound] $ \nav -> do
            let (aAttr, t) = navLink nav
            let liAttrs = if Just nav == nav' then [ class_ "futu-active" ] else []
            li_ liAttrs $ a_ [ aAttr ] $ toHtml t

page_ :: Text -> Maybe Nav -> Html () -> HtmlPage sym
page_ title nav body = do
    Lucid.page_ (title <> " - Prox management") pageParams $ do
        navigation_ nav
        fullRow_ $ header_ $ h1_ $ toHtml title
        row_ $ large_ 12 [ class_ "futu-block" ] body

pageParams :: PageParams
pageParams = defPageParams
    & pageCss    .~ [ css ]
    -- & pageJs     .~ [ $(makeRelativeToProject "checklist.js" >>= embedJS) ]
    -- & pageJQuery .~ True

css :: C.Css
css = do
    for_ [C.body, C.html] $ \el -> el C.? do
        C.backgroundColor $ colourClay $ FutuAccent AF5 AC1

    ".futu-block" C.? do
        C.backgroundColor $ C.grayish 250
        C.paddingTop $ C.em 0.5

    ".top-bar" C.? do
        C.backgroundColor C.white
        C.paddingBottom C.nil
        C.borderBottom C.solid (C.px 2) $ colourClay $ FutuAccent AF1 AC1

    ".top-bar" C.? C.ul C.? do
        C.backgroundColor C.white

    ".menu.horizontal" C.|> C.li C.? do
        C.borderBottom C.solid (C.em 0.3) C.white

    ".menu.horizontal" C.|> (C.li C.# ".futu-active") C.? do
        C.borderBottom C.solid (C.em 0.3) $ colourClay $ FutuAccent AF1 AC2

    C.header C.? do
        C.marginTop $ C.em 0.3
        C.marginBottom $ C.em 0.3
