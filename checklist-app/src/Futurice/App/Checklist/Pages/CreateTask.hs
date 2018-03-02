{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateTask (createTaskPage) where

import Control.Lens              (forOf_, lengthOf, re)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

createTaskPage
    :: World
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "create-task"
createTaskPage world authUser = checklistPage_ "Create task" [] authUser (Just NavCreateTask) $ do
    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "task-create" ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                input_ [ futuId_ "task-name", type_ "text" ]
        row_ $ large_ 12 $
            label_ $ do
                "Info"
                input_ [ futuId_ "task-info", type_ "text" ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                select_ [ futuId_ "task-role" ] $ do
                    optionSelected_ True [ value_ "" ] "-"
                    for_ [ minBound .. maxBound ] $ \role ->
                        optionSelected_ False
                            [ value_ $ role ^. re _TaskRole ]
                            $ toHtml $ role ^. re _TaskRole
        row_ $ large_ 12 $ label_ $ do
            "Comment field"
            br_ []
            checkbox_ False [ futuId_ "task-comment" ]
        row_ $ large_ 12 $ label_ $ do
            "Prerequisites"
            br_ []
            small_ $ i_ "Note: Prerequisites must be also added to the checklist"
            select_ [ futuId_ "task-prereqs", multiple_ "multiple", size_ $ textShow (lengthOf (worldTasks . folded) world) ] $
                forOf_ (worldTasksSortedByName . folded) world $ \t -> do
                    optionSelected_ False
                        [ value_ $ t ^. identifierText ]
                        $ toHtml $ t ^. nameText
        row_ $ large_ 12 $ label_ $ do
            "Tags"
            select_ [ futuId_ "task-tags", multiple_ "multiple" ] $
                for_ [minBound .. maxBound] $ \tag -> do
                    optionSelected_ False
                        [ value_ $ tag ^. re _TaskTag ]
                        $ toHtml tag
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 1"
                checklistSelect "task-checklist-1"
            large_ 6 $ label_ $ do
                "Appliance ("
                a_ [ applianceHelpHref ] "help"
                ")"
                checklistAppliance "task-checklist-appliance-1"
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 2"
                checklistSelect "task-checklist-2"
            large_ 6 $ label_ $ do
                "Appliance ("
                a_ [ applianceHelpHref ] "help"
                ")"
                checklistAppliance "task-checklist-appliance-2"
        row_ $ do
            large_ 6 $ label_ $ do
                "Checklist 3"
                checklistSelect "task-checklist-3"
            large_ 6 $ label_ $ do
                "Appliance ("
                a_ [ applianceHelpHref ] "help"
                ")"
                checklistAppliance "task-checklist-appliance-3"

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"
  where
    checklistSelect :: Monad m => Text -> HtmlT m ()
    checklistSelect n = select_ [ futuId_ n ] $ do
        option_ [ value_ "" ] $ "-"
        for_ (world ^.. worldLists . folded) $ \cl ->
            optionSelected_ False
                [ value_ $ cl ^. identifier . getter identifierToText ]
                $ cl ^. nameHtml

    checklistAppliance :: Monad m => Text -> HtmlT m ()
    checklistAppliance n = input_
        [ futuId_ n, type_ "text", placeholder_ "e.g. not external, helsinki or tampere" ]
