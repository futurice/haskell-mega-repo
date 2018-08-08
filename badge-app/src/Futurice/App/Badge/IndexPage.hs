{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Badge.IndexPage (indexPage) where

import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

import Futurice.App.Badge.API
import Futurice.App.Badge.Markup

-- comment to force template-hasksell recompilation
indexPage :: FUM.Login -> HtmlPage "index-page"
indexPage fu = page_ "Badges" (Just NavHome) $ do
    toHtml fu
    br_ []
    img_ [ recordSrc_ recBadge fu ]
