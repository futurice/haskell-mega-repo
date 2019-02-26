{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.SchedulingRequestPage where

import Futurice.IdMap            (fromFoldable)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Personio as P

schedulingRequestPage :: World -> [P.Employee] -> HtmlPage "scheduling-request-page"
schedulingRequestPage world emps = page_ "Scheduling Requests" (Just NavSchedulingRequest) $ do
    h2_ "Scheduling Requests"
    table_ $ do
        thead_ $ do
            th_ "For Group"
            th_ "Template"
            th_ "By"
            th_ "Created"
            th_ "Add Users"
            th_ "Remove Users"
            th_ "Generate Pdf"
            th_ "Status"
            th_ "Delete"
        tbody_ $ do
            for_ (world ^. worldSchedules) $ \schedule -> tr_ $ do
                td_ (toHtml $ toEmployeeString $ (\e -> idemp ^. ix e . P.employeeFullname) <$> (S.toList $ peopleOnSchedule schedule))
                td_ (toHtml $ schedule ^. scheduleScheduleTemplate)
                td_ (maybe "" (\x -> toHtml $ x ^. P.employeeFullname) (memp ^. at (schedule ^. scheduleCreatedBy)))
                td_ (toHtml $ textShow $ utctDay $ schedule ^. scheduleCreatedOn)
                td_ (select_ [ futuId_ "schedule-employees", multiple_ "multiple", name_ "add-employees"] $ do
                          for_ emps $ \e -> do
                              optionSelected_ False [ value_ (showId $ e ^. P.employeeId)] $ toHtml (e ^. P.employeeFullname))
                td_ (select_ [ futuId_ "schedule-employees", multiple_ "multiple", name_ "remove-employees"] $ do
                          for_ emps $ \e -> do
                              optionSelected_ False [ value_ (showId $ e ^. P.employeeId)] $ toHtml (e ^. P.employeeFullname))
                td_ $ a_ [ class_ "button", recordHref_ schedulePdfGet (schedule ^. scheduleScheduleTemplate)] "Download"
                td_ ""
                td_ $ button_ [ class_ "alert button", type_ "button"] "Delete"
  where
    toEmployeeString [] = ""
    toEmployeeString [a] = a
    toEmployeeString [a,b] = a <> " and " <> b
    toEmployeeString (a:as) = a <> "," <> toEmployeeString as

    memp = M.fromList
            $ catMaybes
            $ map (\emp -> case emp ^. P.employeeLogin of
                             Nothing -> Nothing
                             Just l -> Just (l, emp)) emps
    idemp = fromFoldable emps

    showId (P.EmployeeId i) = textShow i
