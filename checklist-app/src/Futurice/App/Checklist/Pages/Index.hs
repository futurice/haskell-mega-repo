{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Index (indexPage) where

import Control.Lens
       (Getting, filtered, has, hasn't, ifoldMapOf, minimumOf, only, re,
       united)
import Data.Semigroup            (Arg (..))
import Data.Time                 (addDays, diffDays)
import Futurice.Graph            (closure)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Personio
import Futurice.App.Checklist.Types

import qualified Personio as P

indexPage
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> IntegrationData
    -> Maybe Office
    -> Maybe Checklist
    -> Maybe Task
    -> Bool  -- ^ done
    -> Bool  -- ^ old
    -> HtmlPage "indexpage"
indexPage world today authUser@(_fu, _viewerRole) integrationData mloc mlist mtask showDone showOld =
    let employees0 = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
        employees1 = maybe id (\l -> filter (has $ employeeOffice . only l)) mloc $ employees0
        employees2 = maybe id (\cl -> filter (has $ employeeChecklist . only (cl ^. checklistId))) mlist $ employees1
        employees3 = maybe id (filter . taskPredicate) mtask employees2
        employees' = employees3

        taskPredicate :: Task -> Employee -> Bool
        taskPredicate task employee = flip has world $ worldTaskItems
            . ix (employee ^. identifier)
            . ix (task ^. identifier)
            . taskItemPredicate

        -- Single item traversal which we use as a filter in taskPredicate
        taskItemPredicate :: Traversal' AnnTaskItem ()
        taskItemPredicate | showDone   = united
                          | otherwise = _AnnTaskItemTodo . united

        taskInChecklist _task Nothing   = True
        taskInChecklist task (Just cl) = has (checklistTasks . ix (task ^. identifier)) cl

        cutoffDate = addDays (-60) today

        titleParts =
            [ (^. re _Office) <$> mloc
            , (^. nameText) <$> mlist
            , (^. nameText) <$> mtask
            ]

        mlistId :: Maybe ChecklistId
        mlistId = view checklistId <$> mlist

        dateName = case mlistId of
            Nothing                       -> "Due date"
            Just NewEmployeeChecklist     -> "Starting date"
            Just LeavingEmployeeChecklist -> "Leaving date"

    in checklistPage_ "Employees" titleParts authUser (Just NavIndex) $ do
        -- List filtering controls
        row_ $ form_ [ futuId_ "selector", action_ "/", method_ "get" ] $ do
            largemed_ 3 $ label_ $ do
                "Office"
                select_ [ name_ "location"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \loc ->
                        optionSelected_ (Just loc == mloc)
                            [ value_ $ loc ^. re _Office ]
                            $ toHtml $ officeToText loc
            largemed_ 3 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl -> do
                        let cid = cl ^. checklistId
                        optionSelected_ (Just cid == mlistId)
                            [ value_ $ cid ^. re _ChecklistId ]
                            $ toHtml $ cl ^. checklistName
            largemed_ 4 $ label_ $ do
                "Task"
                select_ [ name_ "task" ] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldTasksSortedByName . folded) $ \task ->
                        when (taskInChecklist task mlist) $
                            optionSelected_ (mtask ^? _Just . identifier == Just (task ^. identifier))
                                [ value_ $ task ^. identifier . getter identifierToText ] $ do
                                    task ^. nameHtml
                                    " "
                                    countEmployeesWithTask world task employees2

            largemed_ 1 $ do
                label_ $ do
                    checkbox_ showDone [ name_ "show-done", value_ "true", title_ "Show also people with task already done" ]
                    " done"
                label_ $ do
                    checkbox_ showOld [ name_ "show-old", value_ "true", title_ "Show also people with due date over two month ago" ]
                    " old"

            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                div_ $ button_ [ class_ "button" ] $ "Filter"

        for_ mtask $ \task -> when (hasn't (taskInfo . _Empty) task) $ do
            row_ $ large_ 12 $ do
                b_ "Task info: "
                toHtml $ task ^. taskInfo
                hr_ []

        -- The table
        row_ $ large_ 12 $ sortableTable_ $ do
            thead_ $ tr_ $ do
                th_ [title_ "Status"]                      "S"
                th_ [title_ "Personio"]                    "P"
                th_ [title_ "Office"]                      "Off"
                th_ [title_ "Name" ]                       "Name"
                th_ [title_ "Tribe" ]                      "Tribe"
                mcase mtask
                    (th_ [title_ "Checklist"]              "List")
                    $ \task -> do
                        th_ [title_ "Selected task" ] $ task ^. nameHtml
                        when (has (taskTags . folded) task) $ th_ "Task info"
                        when (task ^. taskComment) $ th_ "Comment"
                        unless (null $ fold $ closure (world ^. worldTasks) (task ^.. taskPrereqs . folded)) $ th_ "Prerequisites"
                -- for_ mtask $ \_task -> th_ [ title_ "Additional info for task + employee" ] "Task info"
                th_ [title_ "due date + offset of next undone task" ] "Next task due"
                th_ [title_ dateName]                      $ toHtml dateName
                th_ [title_ "Confirmed - contract signed"] "Confirmed"
                th_ [title_ "Days till due date"]          "ETA"
                -- viewerItemsHeader viewerRole
                th_ [title_ "Task items todo/done"]        "Tasks"
            tbody_ $ for_ employees' $ \employee -> when (showOld || cutoffDate < employee ^. employeeStartingDay) $ do
                let eid = employee ^. identifier
                let firstFutureDay = employees' ^? folded . employeeStartingDay . filtered (> today)
                let startingDay = employee ^. employeeStartingDay
                let etaClass day = case compare day today of
                        -- TODO: magic numbers
                        LT | day < addDays (- 30) today          -> "eta-far-past"
                           | otherwise                           -> "eta-past"
                        EQ                                       -> "eta-today"
                        GT | maybe False (day <=) firstFutureDay -> "eta-near-future"
                           | day > addDays 30 today              -> "eta-far-future"
                           | otherwise                           -> "eta-future"
                let personioCheck'
                        :: (Monad m, Eq a)
                        => Getting a Employee a
                        -> (P.Employee -> Maybe a)
                        -> (a -> HtmlT m ())
                        -> HtmlT m ()
                    personioCheck' = personioCheck integrationData employee

                tr_ [ class_ $ etaClass $ employee ^. employeeStartingDay ] $ do
                    td_ $ do
                        contractTypeHtml $ employee ^. employeeContractType
                        personioCheck' employeeContractType contractType contractTypeHtml'
                    td_ $ traverse_ toHtml $ employee ^. employeePersonio
                    td_ $ do
                        locationHtml mlistId $ employee ^. employeeOffice
                        personioCheck' employeeOffice (Just . view P.employeeOffice) toHtml
                    td_ $ employeeLink employee
                    td_ $ do
                        case tribeOffices (employee ^. employeeTribe) of
                            [off] | off == employee ^. employeeOffice ->
                                toHtml $ employee ^. employeeTribe
                            -- none, multiple offices,
                            -- or employee and tribe's (single) office are different
                            _ -> do
                                toHtml $ employee ^. employeeTribe
                                " ("
                                locationHtml mlistId $ employee ^. employeeOffice
                                ")"
                        personioCheck' employeeTribe (Just . view P.employeeTribe) toHtml
                    mcase mtask
                        (td_ $ checklistNameHtml mloc (employee ^. employeeChecklist) showDone)
                        $ \task -> do
                            td_ $ shortTaskCheckbox_ world employee task
                            unless (null $ task ^. taskTags) $ td_ $ taskInfo_ task employee integrationData
                            when (task ^. taskComment) $ td_ $ taskCommentInput_ world employee task
                            for_ (closure (world ^. worldTasks) (task ^.. taskPrereqs . folded)) $ \prereqTasks ->
                                td_ $ unless (null prereqTasks) $
                                    ul_ [ class_ "no-bullet" ] $ for_ prereqTasks $ \prereqTask ->
                                        for_ (world ^? worldTaskItems . ix eid . ix (prereqTask ^. identifier)) $ \_ ->
                                            li_ $ shortTaskCheckbox_ world employee prereqTask

                    td_ $ do
                        let predicate t = has (worldTaskItems . ix eid . ix (t ^. identifier) . _AnnTaskItemTodo) world
                        let arg       t = Arg (t ^. taskOffset) t
                        case minimumOf (worldTasks . folded . filtered predicate . getter arg) world of
                            Nothing                -> span_ [ title_ "Archive me!" ] "All done"
                            Just (Arg offset task) -> a_
                                [ indexPageHref mloc mlistId (Just task) showDone showOld
                                , title_ $ "Filter to: " <> task ^. nameText
                                ] $
                                day'_ (addDays offset startingDay)
                    td_ $ do
                        day_ startingDay
                        let f = case employee ^. employeeChecklist  of
                                    NewEmployeeChecklist     -> view P.employeeHireDate
                                    LeavingEmployeeChecklist -> view P.employeeEndDate
                        personioCheck' employeeStartingDay f day_
                    td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
                    td_ $ toHtml $ show (diffDays startingDay today) <> "d"
                    case ifoldMapOf
                        (worldTaskItems . ix eid . ifolded)
                        (toTodoCounter world)
                        world
                      of
                        TodoCounter (Counter i j) _perRole -> do
                            -- Per role tasks are hidden, as there are only IT tasks ATM. (2018-04-19)
                            -- case perRole ^. ix viewerRole of
                            --     Counter a b -> td_ $ toHtml (show a) *> "/" *> toHtml (show b)
                            td_ $ toHtml (show i) *> "/" *> toHtml (show j)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

countEmployeesWithTask :: Monad m => World -> Task -> [Employee] -> HtmlT m ()
countEmployeesWithTask world task = toHtml' . foldMap f
  where
    toHtml' (Counter i j) =
      "(" *> toHtml (show i) *> "/" *> toHtml (show j) *> ")"

    f employee = case world ^? worldTaskItems . ix (employee ^. identifier) . ix (task ^. identifier) of
        Nothing                 -> Counter 0 0
        Just AnnTaskItemTodo {} -> Counter 0 1
        Just AnnTaskItemDone {} -> Counter 1 1
