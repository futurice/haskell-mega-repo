{-# LANGUAGE OverloadedStrings #-}
module Futurice.Lucid.Style where

import Clay
import Prelude hiding (div, rem, span)
import Data.Foldable (for_)

css :: Css
css = do
    ".emphasize" & td ? do
        fontWeight bold
        background ("#eee" :: Color)
    ".empasize2" & td ? do
        fontStyle italic
        background ("#efe" :: Color)
    for_ [h1, h2, h3, h4, li, td, div, span ] $ \el -> el ? do
        fontFamily ["Futurice Regular", "Lucida Grande", "Helvetica Neue", "Helvetica", "Roboto", "Arial"] [sansSerif]
    b ? do
        fontFamily ["Futurice Bold", "Lucida Grande", "Helvetica Neue", "Helvetica", "Roboto", "Arial"] [sansSerif]
        fontWeight bold
    star ? fontSize (pt 11)
    h1 ? do
        fontSize (pt 20)
        marginTop (px 20)
    h2 ? do
        fontSize (pt 15)
        marginTop (px 15)
    h3 ? do
        fontSize (pt 13)
        marginTop (px 10)
    ".login" ? do
        color "#46289A"
        textDecoration underline
    ".personio" ? do
        color "#005A4B"
        textDecoration underline
    ".planmill" ? do
        color "#500A5A"
        textDecoration underline

    -- mimicking foundation styles
    ".select2" ? do
        marginBottom $ rem 1

    for_ [td, th] $ \tdh -> ".condensed" & tdh ? do
        paddingTop $ rem 0.1
        paddingBottom $ rem 0.1
