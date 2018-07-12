{-# LANGUAGE OverloadedStrings #-}
module Futurice.Lucid.Navigation (
    Navigation (..),
    page_,
    navigation_,
    futuriceCss,
    pageParamsWithJS,
  ) where

import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), colourClay)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import qualified Clay                      as C
import qualified Data.Text                 as T
import qualified Futurice.Lucid.Foundation as Lucid

-- | Class for navigation descriptions.
-- Which proxy the service descriptions too
class (Ord nav, Enum nav, Bounded nav) => Navigation nav where
    -- | Service name.
    serviceTitle :: proxy nav -> Text

    -- | Description of the link.
    navLink :: nav -> (Attribute, Text)

    pageParams :: proxy nav -> PageParams
    pageParams _ = defPageParams
        & pageCss .~ [ futuriceCss ]

navigation_
    :: (Monad m, Navigation nav)
    => Maybe nav -> HtmlT m ()
navigation_ nav' = div_ [ class_ "top-bar" ] $ fullRow_ $
    div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "menu horizontal" ] $ do
        for_ [minBound .. maxBound] $ \nav -> do
            let (aAttr, t) = navLink nav
            let liAttrs = if Just nav == nav' then [ class_ "futu-active" ] else []
            li_ liAttrs $ a_ [ aAttr ] $ toHtml t

page_
    :: Navigation nav
    => Text -> Maybe nav -> Html () -> HtmlPage sym
page_ title nav body = do
    Lucid.page_ (title <+> serviceTitle nav) (pageParams nav) $ do
        navigation_ nav
        fullRow_ $ header_ $ h1_ $ toHtml title
        row_ $ large_ 12 [ class_ "futu-block" ] body

pageParamsWithJS :: JS -> proxy nav -> PageParams
pageParamsWithJS js _ = defPageParams
    & pageCss .~ [ futuriceCss ]
    & pageJs .~  [ js ]

futuriceCss :: C.Css
futuriceCss = do
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

(<+>) :: Text -> Text -> Text
x <+> y
    | T.null x  = y
    | T.null y  = x
    | otherwise = x <> " - " <> y
