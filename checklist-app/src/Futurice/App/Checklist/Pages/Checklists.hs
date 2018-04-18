{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklists (checklistsPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (filtered, lengthOf)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

checklistsPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "checklists"
checklistsPage world authUser = checklistPage_ "Checklists" [] authUser (Just NavChecklists) $ do
    -- Table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Checklist name" ] "Checklist"
            th_ [ title_ "Active employees in the list" ] "Employees"

        tbody_ $ for_ lists' $ \c -> tr_ $ do
            let cid = c ^. checklistId
            td_ $ checklistLink c
            td_ $ toHtml $ show $ lengthOf
                (worldEmployees . folded . filtered (\e -> e ^. employeeChecklist == cid))
                world
  where
    lists0 = world ^.. worldLists . folded
    lists' = sortOn (view name) lists0
