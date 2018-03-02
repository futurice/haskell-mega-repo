{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Stats (statsPage) where

import Data.Ord                  (Down (..))
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API               (safeLink)
import Text.Printf               (printf)

import Futurice.App.Checklist.API    (checklistApi, statsPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.Map as DM

data SortedTask = SortedTask Task Counter Counter Counter Counter

statsPage
    :: World -- ^ the world
    -> Day -- ^ today
    -> AuthUser -- ^ logged in userError
    -> SortCriteria
    -> Bool
    -> HtmlPage "stats"
statsPage world today authUser sortCriteria sortDescEmpl = checklistPage_ "Stats" [] authUser (Just NavStats) $ do
    let tasks' = world ^.. worldTasksSortedByName . folded
        archivedEmployees = world ^. worldArchive
        tasksWithArchivedEmployees = swapMapMap $ DM.map (\(ArchivedEmployee _ tm) -> tm) archivedEmployees

        fetchEmployee eid = world ^? worldEmployees . ix eid
        compareResult employee = compare (employee ^. employeeStartingDay) today

        -- Filter past employees from future search and other way round
        filterEmployee SortByActiveFuture employee  = if compareResult employee == LT then Nothing else pure employee
        filterEmployee SortByActivePast employee    = if compareResult employee == GT || compareResult employee == EQ then Nothing else pure employee
        filterEmployee _ employee    = pure employee

        countByArchivedEmployeesForTask :: Identifier Task -> Counter
        countByArchivedEmployeesForTask i = maybe
                                         mempty
                                         (foldMap countArchivedUsers)
                                         (tasksWithArchivedEmployees ^? ix i)

        sortedTask = fmap (\task -> let countByActiveEmployees criteria =
                                            ifoldMap (\eid t ->
                                                        maybe
                                                        mempty
                                                        (\_ -> countUsers t)
                                                        (fetchEmployee eid >>= filterEmployee criteria)) (world ^. worldTaskItems' . ix (task ^. identifier))
                                        activeCountFuture = countByActiveEmployees SortByActiveFuture
                                        activeCountPast = countByActiveEmployees SortByActivePast
                                        archiveCount = countByArchivedEmployeesForTask (task ^. identifier)
                                    in  SortedTask
                                        task
                                        activeCountFuture
                                        activeCountPast
                                        archiveCount
                                        (activeCountFuture <> activeCountPast <> archiveCount)) tasks'
        sortedTask' = sortedTask & (if sortDescEmpl then sortOn $ Down . sortByEmpl sortCriteria else sortOn (sortByEmpl sortCriteria))

    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Task"] "Task"
            th_ [title_ "Active employees todo/done with due date in future"] $
                a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByActiveFuture (if sortCriteria == SortByActiveFuture then not sortDescEmpl else False)] $ do
                "Active Empl"
                br_ []
                "Due date in future"
            th_ [title_ "Active employees todo/done with due date in past"] $
                a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByActivePast (if sortCriteria == SortByActivePast then not sortDescEmpl else False)] $ do
                "Active Empl"
                br_ []
                "Due date in past"
            th_ [title_ "Archived employees todo/done"] $
                a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByArchive (if sortCriteria == SortByArchive then not sortDescEmpl else False)] "Archived Empl"
            th_ [title_ "All employees combined todo/done"] $
                a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByBoth (if sortCriteria == SortByBoth then not sortDescEmpl else False)] "All"

        tbody_ $ for_ sortedTask' $ \(SortedTask task counterActiveFuture counterActivePast counterArchive counterBoth) -> tr_ $ do
            td_ $ taskLink task
            td_ $ do
                showPercentageHtml counterActiveFuture
            td_ $ do
                showPercentageHtml counterActivePast
            td_ $ do
                showPercentageHtml counterArchive
            td_ $ do
                showPercentageHtml counterBoth

showPercentageHtml :: Counter -> HtmlT Identity ()
showPercentageHtml (Counter i j) = do
    toHtml (show i)
    "/"
    toHtml (show j)
    " = "
    if j == 0 then "0" else toHtml (printf "%.01f" (100 * fromIntegral i / fromIntegral j :: Double) :: String)
    "%"

countArchivedUsers :: TaskItem -> Counter
countArchivedUsers TaskItemDone = Counter 1 1
countArchivedUsers TaskItemTodo = Counter 0 1

countUsers :: AnnTaskItem -> Counter
countUsers AnnTaskItemDone {} = Counter 1 1
countUsers AnnTaskItemTodo {} = Counter 0 1

sortByEmpl :: SortCriteria -> SortedTask -> Double
sortByEmpl SortByActiveFuture  (SortedTask _ (Counter i j) _ _ _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j
sortByEmpl SortByActivePast    (SortedTask _ _ (Counter i j) _ _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j
sortByEmpl SortByArchive       (SortedTask _ _ _ (Counter i j) _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j
sortByEmpl SortByBoth          (SortedTask _ _ _ _ (Counter i j)) = if j == 0 then 0 else fromIntegral i/ fromIntegral j
