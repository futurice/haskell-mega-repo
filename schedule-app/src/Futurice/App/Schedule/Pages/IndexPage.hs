{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.IndexPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.World

indexPage :: World -> HtmlPage "indexpage"
indexPage world = page_ "Home" (Just NavHome) $ do
    div_ [] $
        toHtml $ show world
