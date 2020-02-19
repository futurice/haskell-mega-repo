{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (
    checklistPage,
    checklistGraph,
    ) where

import Algebra.Graph.Class       (edges, overlay, vertices)
import Control.Lens
       (contains, filtered, foldMapOf, forOf_, has, re)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.Graph             (Graph (..))

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

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
checklistPage world today authUser checklist = checklistPage_ (view nameText checklist <> " - checklist") [] authUser Nothing $ do
    -- Add Task
    subheader_ "Add task"
    futuForm_ "task-add" [ data_ "futu-checklist-id" $ checklist ^. checklistId . re _ChecklistId  ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Task"
                select_ [ futuId_ "task-id" ] $ do
                    optionSelected_ True [ value_ "" ] "-"
                    forOf_ (worldTasksSortedByName . folded) world $ \task -> option_
                        [ value_ $ task ^. identifierText ]
                        $ task ^. nameHtml

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Add"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Tasks
    subheader_ "Tasks"
    row_ $ large_ 12 $ p_ $ toHtml $ "There are " <> textShow (length tasks) <> " tasks"

    -- TODO: move to Markup: tasksList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]                       "Task"
            th_ [ title_ "Info", style_ "max-width: 20em;" ] "Info"
            th_ [ title_ "Role" ]                       "Role"
            th_ [ title_ "Offset" ]                     $ toHtmlRaw ("Day&nbsp;offset" :: Text)
            th_ [ title_ "To whom this task applies" ]  "Applicability"
            th_ [ title_ "Direct prerequisites" ]       "Prerequisites"
            th_ [ title_ "Tags added to task" ]         "Tags"
            th_ [ title_ "Active employees todo/done" ] "Employees"
            th_ [ title_ "Other checklists with the task" ] "Checklists"
            th_ [ title_ "Remove task from the checklist" ] "Remove"

        tbody_ $ for_ tasks $ \task -> tr_ $ do
            let tid = task ^. identifier
            let prereqTasks = task ^.. taskPrereqs . folded
                    -- tasks in this checklist
                    . filtered (\prereqTid -> has (checklistTasks . ix prereqTid) checklist)
                    -- tid -> tasks
                    . getter (\prereqTid -> world ^? worldTasks . ix prereqTid)
                    . _Just

            td_ $ taskLink task
            td_ [ style_ "max-width: 20em;" ] $ small_ $ toHtml $ task ^. taskInfo
            td_ $ roleHtml mcid (task ^. taskRole)
            td_ $ toHtml $ show $ task ^. taskOffset
            td_ $ toHtml $ task ^. taskApplicability
            td_ $ unless (null prereqTasks) $ ul_ $
                for_ (prereqTasks ^. tasksSorted world) $ \prereqTask -> li_ $ do
                    taskLink prereqTask
                    when (prereqTask ^. taskOffset > task ^. taskOffset) $ do
                        span_ [class_ "label alert"] $ do
                            "Later offset "
                            toHtml $ show $ prereqTask ^. taskOffset
            td_ $ ul_ $ for_ (task ^. taskTags) $ \tag -> do
                li_ $ toHtml tag
            td_ $ a_ [ indexPageHref Nothing mcid (Just tid) False False ] $
                case foldMapOf (worldTaskItems' . ix tid . folded) countUsers world of
                    Counter i j ->
                        toHtml (show i) *> "/" *> toHtml (show j)
            td_ $ forWith_
                (br_ [])
                (world ^.. worldLists . folded .  filtered (\l -> has (checklistTasks . ix tid) l && l ^. checklistId /= checklist ^. checklistId))
                checklistLink
            td_ $ button_
                [ class_ "button alert", futuId_ "task-remove"
                , data_ "futu-checklist-id" $ checklist ^. checklistId . re _ChecklistId
                , data_ "futu-task-id" $ task ^. identifierText
                ]
                "Remove"

    -- Tasks graph
    subheader_ $ "Tasks graph"
    fullRow_ $ img_ [ checklistGraphSrc $ checklist ^. checklistId ]

    -- Employees
    subheader_ "Employees"
    -- TODO: mvoe to Markup: employeeList
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                      "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            let startingDay = employee ^. employeeStartingDay
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml Nothing $ employee ^. employeeOffice
            td_ $ employeeLink employee
            td_ $ toHtml $ show startingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            td_ $ toHtml $ show (diffDays startingDay today) <> " days"
  where
    tasks0 = world ^.. worldTasksSorted (authUser ^. authUserTaskRole) . folded
    tasks = filter (\task -> checklist ^. checklistTasks . contains (task ^. identifier)) tasks0

    mcid = checklist ^? checklistId

    countUsers AnnTaskItemDone {} = Counter 1 1
    countUsers AnnTaskItemTodo {} = Counter 0 1

    employees =  sortOn (view employeeStartingDay)
        $ filter (\e -> e ^. employeeChecklist == checklist ^. checklistId)
        $ toList (IdMap.toMap (world ^. worldEmployees))

-------------------------------------------------------------------------------
-- Graph
-------------------------------------------------------------------------------

checklistGraph :: World -> Checklist -> Graph TaskNode "checklist"
checklistGraph world checklist = Graph $ overlay vs es
  where
    es = edges
        [ (taskNode prereqTask, taskNode task)
        | task <- tasks
        , prereqTid <- task ^.. taskPrereqs . folded
        , checklist ^. checklistTasks . contains prereqTid
        , prereqTask <- world ^.. worldTasks . ix prereqTid
        ]
    vs = vertices $ tasks ^.. folded . getter taskNode

    tasks = checklist ^..  checklistTasks . folded .
        getter (\tid -> world ^? worldTasks . ix tid) . _Just
