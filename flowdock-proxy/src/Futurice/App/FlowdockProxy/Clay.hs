{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.Clay (css) where

import Clay

css :: Css
css = do
    ".nick-0" ? do
        color "#990000"
    ".nick-1" ? do
        color "#009900"
    ".nick-2" ? do
        color "#000099"
    ".nick-3" ? do
        color "#666600"
    ".nick-4" ? do
        color "#006666"
    ".nick-5" ? do
        color "#660066"
    ".nick-6" ? do
        color "#336699"
    ".nick-7" ? do
        color "#993366"
