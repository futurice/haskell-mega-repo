{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Clay where

import Clay
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), colourClay)
import Futurice.Lucid.Foundation
       (PageParams, defPageParams, embedJS, pageCss, pageJQuery, pageJs)
import Futurice.Prelude          hiding ((&))
import Prelude ()

import qualified Control.Lens as L

pageParams :: PageParams
pageParams = defPageParams
    L.& pageCss    .~ [ css ]
    L.& pageJs     .~ [ $(makeRelativeToProject "checklist.js" >>= embedJS) ]
    L.& pageJQuery .~ True

css :: Css
css = do
    label # ".error" ? do
        color red
    "input[type=text]" # ".error" ? do
        borderColor red
    "input[type=date]" # ".error" ? do
        borderColor red
    "select" # ".error" ? do
        borderColor red
    table ? tbody ? tr ? do
        nthChild "even" & do
            ".eta-far-past"    & backgroundColor (lighten 0.92 blue)
            ".eta-past"        & backgroundColor (lighten 0.92 green)
            ".eta-today"       & backgroundColor (lighten 0.92 yellow)
            ".eta-near-future" & backgroundColor (lighten 0.90 orange)
            ".eta-future"      & backgroundColor (lighten 0.92 red)
            ".eta-far-future"  & backgroundColor (lighten 0.92 violet)
        nthChild "odd" & do
            ".eta-far-past"    & backgroundColor (lighten 0.95 blue)
            ".eta-past"        & backgroundColor (lighten 0.95 green)
            ".eta-today"       & backgroundColor (lighten 0.95 yellow)
            ".eta-near-future" & backgroundColor (lighten 0.92 orange)
            ".eta-future"      & backgroundColor (lighten 0.95 red)
            ".eta-far-future"  & backgroundColor (lighten 0.95 violet)

    -- Task appliance classes
    "span" # ".contract" ? do
        color "#600"
    "span" # ".location" ? do
        color "#060"

    for_ [body, html] $ \el -> el ? do
        backgroundColor $ colourClay $ FutuAccent AF5 AC1

    ".futu-block" ? do
        backgroundColor $ grayish 250
        paddingTop $ em 0.5

    -- fancier topbar
    ".top-bar" ? do
        backgroundColor white
        paddingBottom Clay.nil
        borderBottom solid (px 2) $ colourClay $ FutuAccent AF1 AC1

    ".top-bar" ? ul ? do
        backgroundColor white

    ".menu.horizontal" |> li ? do
        borderBottom solid (em 0.3) white

    ".menu.horizontal" |> (li # ".futu-active") ? do
        borderBottom solid (em 0.3) $ colourClay $ FutuAccent AF1 AC2

    header ? do
        marginTop $ em 0.3
        marginBottom $ em 0.3
