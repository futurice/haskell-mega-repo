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

data SortedTask = SortedTask Task Counter Counter

statsPage
    :: World -- ^ the world
    -> AuthUser -- ^ logged in userError
    -> Bool
    -> HtmlPage "stats"
statsPage world authUser sortDescEmpl =
    let countUsers AnnTaskItemDone {} = Counter 1 1
        countUsers AnnTaskItemTodo {} = Counter 0 1

        sortByEmpl (SortedTask _ (Counter i j) _) = if j == 0 then 0 else fromIntegral i/ fromIntegral j :: Double

        tasks' = world ^.. worldTasksSortedByName . folded
        archivedEmployees = world ^. worldArchive
        tasksWithArchivedEmployees = swapMapMap $ DM.map (\(ArchivedEmployee _ tm) -> tm) archivedEmployees

        archivedEmployeesCount = length archivedEmployees

        findArchivedEmployeesForTask :: Identifier Task -> Counter
        findArchivedEmployeesForTask i = Counter
                                         (maybe 0 length (tasksWithArchivedEmployees ^? ix i))
                                         archivedEmployeesCount

        sortedTask = fmap (\task ->
                             SortedTask
                             task
                             (foldMapOf (worldTaskItems' . ix (task ^. identifier) . folded) countUsers world)
                             (findArchivedEmployeesForTask (task ^. identifier))) tasks'
        sortedTask' = sortedTask & (if sortDescEmpl then sortOn $ Down . sortByEmpl else sortOn sortByEmpl)

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
               th_ [title_ "Active employees todo/done"] $ a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint (not sortDescEmpl)] "Active Empl"
               th_ [title_ "Archived employees todo/done"] $ "Archived Empl"
               th_ [title_ "Combined employees todo/done"] $ "Both"

           tbody_ $ for_ sortedTask' $ \(SortedTask task (Counter i j) (Counter archi archj)) -> tr_ $ do
               td_ $ taskLink task
               td_ $ do
                   showPercentageHtml i j
               td_ $ do
                   showPercentageHtml archi archj
               td_ $ do
                   showPercentageHtml (i + archi) (j + archj)
