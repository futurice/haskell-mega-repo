{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Stats (statsPage) where

import Control.Lens              (foldMapOf)
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

data SortedTask = SortedTask Task Counter Counter Counter

statsPage
    :: World -- ^ the world
    -> AuthUser -- ^ logged in userError
    -> SortCriteria
    -> Bool
    -> HtmlPage "stats"
statsPage world authUser sortCriteria sortDescEmpl =
    let countUsers AnnTaskItemDone {} = Counter 1 1
        countUsers AnnTaskItemTodo {} = Counter 0 1

        sortByEmpl SortByActive  (SortedTask _ (Counter i j) _ _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j :: Double
        sortByEmpl SortByArchive (SortedTask _ _ (Counter i j) _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j :: Double
        sortByEmpl SortByBoth    (SortedTask _ _ _ (Counter i j)) = if j == 0 then 0 else fromIntegral i/ fromIntegral j :: Double

        tasks' = world ^.. worldTasksSortedByName . folded
        archivedEmployees = world ^. worldArchive
        tasksWithArchivedEmployees = swapMapMap $ DM.map (\(ArchivedEmployee _ tm) -> tm) archivedEmployees

        archivedEmployeesCount = length archivedEmployees

        findArchivedEmployeesForTask :: Identifier Task -> Counter
        findArchivedEmployeesForTask i = Counter
                                         (maybe 0 length (tasksWithArchivedEmployees ^? ix i))
                                         archivedEmployeesCount

        sortedTask = fmap (\task -> let activeCount = (foldMapOf (worldTaskItems' . ix (task ^. identifier) . folded) countUsers world)
                                        archiveCount = (findArchivedEmployeesForTask (task ^. identifier))
                                    in  SortedTask
                                        task
                                        activeCount
                                        archiveCount
                                        (activeCount <> archiveCount)) tasks'
        sortedTask' = sortedTask & (if sortDescEmpl then sortOn $ Down . (sortByEmpl sortCriteria) else sortOn (sortByEmpl sortCriteria))

        showPercentageHtml i j = do
            toHtml (show i)
            "/"
            toHtml (show j)
            " = "
            if j == 0 then "0" else toHtml (printf "%.01f" (100 * fromIntegral i / fromIntegral j :: Double) :: String)
            "%"

    in checklistPage_ "Stats" authUser $ do

       header "Stats" []
       row_ $ large_ 12 $ table_ $ do
           thead_ $ tr_ $ do
               th_ [title_ "Task"] "Task"
               th_ [title_ "Active employees todo/done"] $
                   a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByActive (if sortCriteria == SortByActive then not sortDescEmpl else False)] "Active Empl"
               th_ [title_ "Archived employees todo/done"] $
                   a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByArchive (if sortCriteria == SortByArchive then not sortDescEmpl else False)] "Archived Empl"
               th_ [title_ "Combined employees todo/done"] $
                   a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint SortByBoth (if sortCriteria == SortByBoth then not sortDescEmpl else False)] "Both"

           tbody_ $ for_ sortedTask' $ \(SortedTask task (Counter i j) (Counter archi archj) (Counter bothi bothj)) -> tr_ $ do
               td_ $ taskLink task
               td_ $ do
                   showPercentageHtml i j
               td_ $ do
                   showPercentageHtml archi archj
               td_ $ do
                   showPercentageHtml (bothi) (bothj)
