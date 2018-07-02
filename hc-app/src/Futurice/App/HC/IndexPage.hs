{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.IndexPage (indexPage) where

import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

import Futurice.App.HC.Markup

-- comment to force template-hasksell recompilation
indexPage :: FUM.Login -> HtmlPage "index-page"
indexPage _fu = page_ "HC app" (Just NavHome) $ do
    fullRow_ $ ul_ $ do
        li_ $ do
            a_ [href_ "/personio-validation"] "Personio validations"
            " - report about incorrect or missing data in Personio"
        li_ $
            a_ [href_ "/early-caring"] "Early caring report"
        li_ $ do
            a_ [href_ "/anniversaries"] "Anniversaries"
            " - decades at Futurice or on Earth"
        li_ $ do
            a_ [href_ "/hr-numbers"] "HR Numbers"
            " - simple table of employees' HR numbers"
        li_ $ do
            a_ [href_ "/private-contacts"] "Private concacts"
            " - people private email and phone number on a single page"
