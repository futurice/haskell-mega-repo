{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (checklistPage) where

import Control.Lens              (filtered, foldMapOf, forOf_, has)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.TaskTag (taskTagToText)

import qualified Futurice.IdMap as IdMap

-- |
--
-- === Preconditions
--
-- * 'Checklist' is in the 'World'.
checklistPage
    :: World
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Checklist
    -> HtmlPage "checklist"
checklistPage world today authUser checklist = checklistPage_ (view nameText checklist <> " - checklist") authUser $ do
    header (checklist ^. nameText <> " - checklist") []

    -- Edit
    futuForm_"checklist-edit" [ data_ "futu-checklist-id" $ checklist ^. identifierText ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                let v = checklist ^. nameText
                input_ [ futuId_ "checklist-name", type_ "text", value_ v ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Save"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Add Task
    subheader_ "Add task"
    futuForm_ "task-add" [ data_ "futu-checklist-id" $ checklist ^. identifierText  ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Task"
                select_ [ futuId_ "task-id" ] $ do
                    optionSelected_ True [ value_ "" ] "-"
                    forOf_ (worldTasksSortedByName . folded) world $ \task -> option_
                        [ value_ $ task ^. identifierText ]
                        $ task ^. nameHtml

        row_ $ large_ 12 $
            label_ $ do
                "Appliance ("
                a_ [ applianceHelpHref ] "help"
                ")"
                input_ [ futuId_ "task-appliance", type_ "text", value_ "", placeholder_ "e.g. helsinki or tampere, permanent or fixed-term, external" ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Add"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Tasks
    subheader_ "Tasks"
    -- TODO: move to Markup: tasksList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]                       "Task"
            th_ [ title_ "Info", style_ "max-width: 20em;" ] "Info"
            th_ [ title_ "Role" ]                       "Role"
            th_ [ title_ "Direct prerequisites" ]       "Prerequisites"
            th_ [ title_ "Tags added to task" ]         "Tags"
            th_ [ title_ "Active employees todo/done" ] "Empl"
            th_ [ title_ "To whom this task applies" ]  "Appliance"
            th_ [ title_ "Other checklists with the task" ] "Other checklists"
            th_ [ title_ "Remove task from the checklist" ] "Remove"

        tbody_ $ forOf_ (worldTasksSorted (authUser ^. authUserTaskRole) . folded) world $ \task -> do
            let tid = task ^. identifier
            for_ (checklist ^? checklistTasks . ix tid) $ \app -> tr_ $ do
                td_ $ taskLink task
                td_ [ style_ "max-width: 20em;" ] $ small_ $ toHtml $ task ^. taskInfo
                td_ $ roleHtml mlist (task ^. taskRole)
                td_ $ forOf_ (taskPrereqs . folded . getter (\tid' -> world ^. worldTasks . at tid') . _Just) task $ \prereqTask -> do
                    let prereqTid = prereqTask ^. identifier
                    for_ (checklist ^? checklistTasks . ix prereqTid) $ \_ -> do
                        taskLink prereqTask
                        br_ []
                td_ $ for_ (task ^. taskTags) $ \tag -> do
                    toHtml $ taskTagToText tag
                    br_ []
                td_ $ a_ [ indexPageHref Nothing mlist (Just tid) False False ] $
                    case foldMapOf (worldTaskItems' . ix tid . folded) countUsers world of
                        Counter i j ->
                            toHtml (show i) *> "/" *> toHtml (show j)
                td_ $ toHtml app
                td_ $ forWith_
                    (br_ [])
                    (world ^.. worldLists . folded .  filtered (\l -> has (checklistTasks . ix tid) l && l ^. identifier /= checklist ^. identifier))
                    checklistLink
                td_ $ button_
                    [ class_ "button alert", futuId_ "task-remove"
                    , data_ "futu-checklist-id" $ checklist ^. identifierText
                    , data_ "futu-task-id" $ task ^. identifierText
                    ]
                    "Remove"

    -- Employees
    subheader_ "Employees"
    -- TODO: mvoe to Markup: employeeList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            let startingDay = employee ^. employeeStartingDay
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeOffice
            td_ $ employeeLink employee
            td_ $ toHtml $ show startingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            td_ $ toHtml $ show (diffDays startingDay today) <> " days"


  where
    mlist = Just checklist

    countUsers AnnTaskItemDone {} = Counter 1 1
    countUsers AnnTaskItemTodo {} = Counter 0 1

    employees =  sortOn (view employeeStartingDay)
        $ filter (\e -> e ^. employeeChecklist == checklist ^. identifier)
        $ toList (IdMap.toMap (world ^. worldEmployees))
