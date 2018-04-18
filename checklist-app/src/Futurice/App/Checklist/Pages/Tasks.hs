{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Tasks (tasksPage) where

import Control.Lens
       (contains, filtered, foldMapOf, forOf_, has, re)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Text.Printf               (printf)

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

tasksPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> Maybe TaskRole
    -> Maybe Checklist
    -> HtmlPage "tasks"
tasksPage world authUser@(_fu, _viewerRole) mrole mlist =
    let tasks0 = world ^.. worldTasksSortedByName . folded
        tasks1 = maybe id (filter . rolePredicate) mrole tasks0
        tasks2 = maybe id (filter . checklistPredicate) mlist tasks1
        tasks' = tasks2

        rolePredicate :: TaskRole -> Task -> Bool
        rolePredicate role task = role == task ^. taskRole

        checklistPredicate :: Checklist -> Task -> Bool
        checklistPredicate cl task = fromMaybe False $
            world ^? worldLists . ix (cl ^. checklistId) . checklistTasks . contains (task ^. identifier)

        titleParts =
            [ (^. re _TaskRole) <$> mrole
            , (^. nameText ) <$> mlist
            ]

    in checklistPage_ "Tasks"  titleParts authUser (Just NavTasks) $ do
        -- List filtering controls
        row_ $ form_ [ futuId_ "selector", action_ $ "/tasks", method_ "get" ] $ do
            largemed_ 3 $ label_ $ do
                "Role"
                select_ [ name_ "role"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \role ->
                        optionSelected_ (Just role == mrole)
                            [ value_ $ role ^. re _TaskRole ]
                            $ toHtml $ role ^. re _TaskRole
            largemed_ 8 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl ->
                        optionSelected_ (Just cl == mlist)
                            [ value_ $ cl ^. checklistId . re _ChecklistId ]
                            $ cl ^. nameHtml
            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                button_ [ class_ "button" ] $ "Filter"

        -- The table
        row_ $ large_ 12 $ sortableTable_ $ do
            thead_ $ tr_ $ do
                th_ [ title_ "Task" ]                       "Task"
                th_ [ title_ "Info", style_ "max-width: 20em;" ] "Info"
                th_ [ title_ "Role" ]                       "Role"
                th_ [ title_ "Offset in days" ]             "Day Offset"
                th_ [ title_ "Applicability" ]              "Applicability"
                th_ [ title_ "Direct prerequisites" ]       "Prerequisites"
                th_ [ title_ "Tags added to task" ]         "Tags"
                th_ [ title_ "Active employees todo/done" ] "Empl"
                th_ [ title_ "Checklists with the task" ]   "Checklists"

            tbody_ $ for_ tasks' $ \task -> tr_ $ do
                let tid = task ^. identifier


                td_ $ taskLink task
                td_ [ style_ "max-width: 20em;" ] $ small_ $ toHtml $ task ^. taskInfo
                td_ $ roleHtml (mlist ^? _Just . checklistId) $ task ^. taskRole
                td_ $ toHtml $ show $ task ^. taskOffset
                td_ $ toHtml $ task ^. taskApplicability
                td_ $ forOf_ (taskPrereqs . folded . getter (\tid' -> world ^. worldTasks . at tid') . _Just) task $ \prereqTask -> do
                    taskLink prereqTask
                    br_ []
                td_ $ ul_ $ for_ (task ^. taskTags) $ \tag -> do
                    li_ $ toHtml tag
                td_ $ a_ [ indexPageHref Nothing (mlist ^? _Just . checklistId) (Just tid) defaultShowAll False ] $
                    case foldMapOf (worldTaskItems' . ix tid . folded) countUsers world of
                        Counter i j -> do
                            toHtml (show i)
                            "/"
                            toHtml (show j)
                            " = "
                            if j == 0 then "0" else toHtml (printf "%.01f" (100 * fromIntegral i / fromIntegral j :: Double) :: String)
                            "%"
                td_ $ forWith_
                    (br_ [])
                    (world ^.. worldLists . folded . filtered (has (checklistTasks . ix tid)))
                    checklistLink
 where
  countUsers AnnTaskItemDone {} = Counter 1 1
  countUsers AnnTaskItemTodo {} = Counter 0 1
