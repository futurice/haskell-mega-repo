{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Clay where

import Prelude ()
import Futurice.Prelude          hiding ((&))
import Clay
import Futurice.Lucid.Foundation
       (PageParams, defPageParams, embedJS, pageCss, pageJQuery,
       pageJs)

import qualified Control.Lens as L

pageParams :: PageParams
pageParams = defPageParams
    L.& pageCss    .~ [ css ]
    L.& pageJs     .~ [ $(makeRelativeToProject "checklist.js" >>= embedJS) ]
    L.& pageJQuery .~ True


css :: Css
css = do
    header ? do
        marginTop $ em 1
        marginBottom $ em 1
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
