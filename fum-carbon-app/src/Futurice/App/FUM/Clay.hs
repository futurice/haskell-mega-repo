{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.FUM.Clay where

import Clay
import Futurice.Lucid.Foundation
       (PageParams, defPageParams, embedJS, pageCss, pageJQuery, pageJs)
import Futurice.Prelude          hiding ((&), (**))
import Prelude ()

import qualified Control.Lens as L

pageParams :: PageParams
pageParams = defPageParams
    L.& pageCss    .~ [ css ]
    L.& pageJs     .~ [ $(embedJS "fum-carbon.js") ]
    L.& pageJQuery .~ True

css :: Css
css = do
    header ? do
        marginTop $ em 1
        marginBottom $ em 1

    label # ".error" ? do
        color red
        "input[type=text]" ? do
            borderColor red
        "input[type=date]" ? do
            borderColor red
        "select" ? do
            borderColor red
        ".select2-container--default .select2-selection--single" ? do
            borderColor red

    label # ".pending" ? do
        color orange
        "input[type=text]" ?  do
            borderColor orange
        "input[type=date]" ? do
            borderColor orange
        "select" ? do
            borderColor orange
        ".select2-container--default .select2-selection--single" ? do
            borderColor orange

    "td button.button" ? do
        sym margin (em 0)

    Clay.span # ".alert" ? do
        color "#cc4b37"

    Clay.span # ".warning" ? do
        color "#ffae00"

    "div.futu-block" ? do
        marginBottom (em 1)

        h2 ? do
            borderBottom solid (px 2) "#ccc"
