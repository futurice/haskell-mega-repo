{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.IndexPage (indexPage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

indexPage :: FUM.Login -> HtmlPage "index-page"
indexPage fu = page_ "HC app" $ do
    fullRow_ $ h1_ "HC"

    fullRow_ $ do
        "Hello " <> toHtml fu
        br_ []
        ul_ $ do
            li_ $ do
                a_ [href_ "/personio-validation"] "Personio validations"
                " - report about incorrect or missing data in Personio"
            li_ $ do
                a_ [href_ "/private-contacts"] "Private concacts"
                " - people private email and phone number on a single page"
            li_ $
                a_ [href_ "/early-caring"] "Early caring report"
