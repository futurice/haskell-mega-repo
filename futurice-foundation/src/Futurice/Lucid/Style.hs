{-# LANGUAGE OverloadedStrings #-}
module Futurice.Lucid.Style where

import Clay
import Prelude hiding (div, rem, span)
import Data.Foldable (for_)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), colourClay)

css :: Css
css = do
    for_ [h1, h2, h3, h4, li, td, div, span ] $ \el -> el ? do
        fontFamily ["Futurice Regular", "Lucida Grande", "Helvetica Neue", "Helvetica", "Roboto", "Arial"] [sansSerif]
    b ? do
        fontFamily ["Futurice Bold", "Lucida Grande", "Helvetica Neue", "Helvetica", "Roboto", "Arial"] [sansSerif]
        fontWeight bold
    star ? fontSize (pt 11)
    h1 ? do
        fontSize (pt 20)
        marginTop (rem 0.5)
    h2 ? do
        fontSize (pt 15)
        marginTop (rem 0.4)
    h3 ? do
        fontSize (pt 13)
        marginTop (rem 0.3)

    ".login" ? do
        color $ colourClay $ FutuAccent AF2 AC2
        textDecoration underline
    ".personio" ? do
        color $ colourClay $ FutuAccent AF3 AC3
        textDecoration underline
    ".planmill" ? do
        color $ colourClay $ FutuAccent AF2 AC3
        textDecoration underline

    -- mimicking foundation styles
    ".select2" ? do
        marginBottom $ rem 1

    for_ [td, th] $ \tdh -> ".condensed" & tdh ? do
        paddingTop $ rem 0.1
        paddingBottom $ rem 0.1
