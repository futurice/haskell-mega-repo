{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Archive (archivePage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.Links             (safeLink)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

archivePage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "archive"
archivePage world authUser@(_, viewerRole) = checklistPage_ "Archive" [] authUser (Just NavMore) $ do
    let employees = sortOn (view $ archiveEmployee . employeeStartingDay) $ world ^.. worldArchive . folded

    -- The table
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                      "Office"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            viewerItemsHeader viewerRole
            th_ [title_ "Task items todo/done"]        "Tasks"
            th_                                        "Audit"
        tbody_ $ for_ employees $ \(ArchivedEmployee employee taskMap) -> tr_ $ do
            let (TodoCounter (Counter i j) perRole) = ifoldMap (taskItemtoTodoCounter world) taskMap

            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml Nothing $ employee ^. employeeOffice
            td_ $ employee ^. nameHtml
            td_ $ checklistNameHtml Nothing (employee ^. employeeChecklist) False
            td_ $ toHtml $ show $ employee ^. employeeStartingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            case perRole ^. ix viewerRole of
                Counter a b -> do
                    td_ $ toHtml (show a) *> "/" *> toHtml (show b)
                    td_ $ toHtml (show i) *> "/" *> toHtml (show j)
            td_ $ a_ [ href_ $ linkToText $ safeLink checklistApi employeeAuditPageEndpoint (employee ^. identifier) ] "Audit log"
