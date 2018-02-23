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

data SortedTask = SortedTask Task Counter

statsPage
    :: World -- ^ the world
    -> AuthUser -- ^ logged in userError
    -> Bool
    -> HtmlPage "stats"
statsPage world authUser sortDescEmpl =
    let countUsers AnnTaskItemDone {} = Counter 1 1
        countUsers AnnTaskItemTodo {} = Counter 0 1

        sortByEmpl (SortedTask _ (Counter i j)) = if j == 0 then 0 else fromIntegral i/ fromIntegral j :: Double

        tasks' = world ^.. worldTasksSortedByName . folded
        sortedTask = fmap (\task -> SortedTask task $ foldMapOf (worldTaskItems' . ix (task ^. identifier) . folded) countUsers world ) tasks'
        sortedTask' = sortedTask & (if sortDescEmpl then sortOn $ Down . sortByEmpl else sortOn sortByEmpl)
    in checklistPage_ "Stats" authUser $ do

       header "Stats" []
       row_ $ large_ 12 $ table_ $ do
           thead_ $ tr_ $ do
               th_ [title_ "Task"] "Task"
               th_ [title_ "Employees todo/done"] $ a_ [href_ $ linkToText $ safeLink checklistApi statsPageEndpoint (not sortDescEmpl)] "Empl"

           tbody_ $ for_ sortedTask' $ \(SortedTask task (Counter i j)) -> tr_ $ do
               td_ $ taskLink task
               td_ $ do
                   toHtml (show i)
                   "/"
                   toHtml (show j)
                   " = "
                   if j == 0 then "0" else toHtml (printf "%.01f" (100 * fromIntegral i / fromIntegral j :: Double) :: String)
                   "%"
