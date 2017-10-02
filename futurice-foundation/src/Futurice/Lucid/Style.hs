{-# LANGUAGE OverloadedStrings #-}
module Futurice.Lucid.Style where

import Clay
import Prelude hiding (div, rem, span)

css :: Css
css = do
    ".emphasize" & td ? do
        fontWeight bold
        background ("#eee" :: Color)
    ".empasize2" & td ? do
        fontStyle italic
        background ("#efe" :: Color)
    h1 ? h2 ? h3 ? h4? li ? td ? div ? span ? b ? do
        fontFamily ["Lucida Grande", "Helvetica", "Arial"] [sansSerif]
    star ? fontSize (pt 11)
    h1 ? do
        fontSize (pt 20)
        fontWeight bold
    h2 ? do
        fontSize (pt 15)
        fontWeight bold
    h3 ? do
        fontSize (pt 13)
        fontWeight bold
    ".login" ?
        color "#46289A"
    ".personio" ?
        color "#005A4B"
    ".planmill" ?
        color "#500A5A"

    -- mimicking foundation styles
    ".select2" ? do
        marginBottom $ rem 1
