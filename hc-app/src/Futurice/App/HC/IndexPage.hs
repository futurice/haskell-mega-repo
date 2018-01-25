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

    fullRow_ $ "Hello " <> toHtml fu
