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
        marginTop (px 20)
    h2 ? do
        fontSize (pt 15)
        fontWeight bold
        marginTop (px 15)
    h3 ? do
        fontSize (pt 13)
        fontWeight bold
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
