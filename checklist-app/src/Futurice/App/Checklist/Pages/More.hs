{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.More (morePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

morePage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "more"
morePage _world authUser = checklistPage_ "More things" [] authUser (Just NavMore) $
    ul_ $ do
        li_ $ a_ [ recordHref_ routeArchive ] "Archive"
        li_ $ a_ [ recordHref_ routeAgents ] "Active agents"
        li_ $ a_ [ recordHref_ routeStats  SortByActivePast True False ] "Stats"
        li_ $ a_ [ recordHref_ routeHelpAppliance ] "Help: Applicabilities"
        li_ $ a_ [ recordHref_ routeHelpServices ] "Services info"
        li_ $ a_ [ recordHref_ routeCreateEmployee Nothing Nothing Nothing ] "Raw (not from Personio) employee creation"
