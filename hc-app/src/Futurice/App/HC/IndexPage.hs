{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.IndexPage (indexPage) where

import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

import Futurice.App.HC.API
import Futurice.App.HC.Markup

-- comment to force template-hasksell recompilation
indexPage :: FUM.Login -> HtmlPage "index-page"
indexPage _fu = page_ "HC app" (Just NavHome) $ do
    fullRow_ $ ul_ $ do
        li_ $ do
            a_ [recordHref_ recPersonioValidations] "Personio validations"
            " - report about incorrect or missing data in Personio"
        li_ $
            a_ [recordHref_ recEarlyCaring] "Early caring report"
        li_ $ do
            a_ [recordHref_ recAnniversaries] "Anniversaries"
            " - decades at Futurice or on Earth"
        li_ $ do
            a_ [recordHref_ recHrNumbers] "HR Numbers"
            " - simple table of employees' HR numbers"
        li_ $ do
            a_ [recordHref_ recPrivateContacts] "Private concacts"
            " - people private email and phone number on a single page"
