{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.SchedulingRequestPage where

import Futurice.IdMap            (fromFoldable, key)
import Futurice.Lomake
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.AddEmployeesToSchedule
import Futurice.App.Schedule.Command.RemoveEmployeesFromSchedule
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
            th_ [ style_ "width: 18%"] "Add Users"
            th_ [ style_ "width: 18%"] "Remove Users"
            th_ "Generate Pdf"
            th_ "Status"
            th_ "Delete"
        tbody_ $ do
            for_ (world ^. worldSchedules) $ \schedule -> tr_ $ do
                td_ $ toHtml $ toEmployeeString $ (\e -> idemp ^. ix e . P.employeeFullname) <$> (S.toList $ peopleOnSchedule schedule)
                td_ $ toHtml $ schedule ^. scheduleTemplateName
                td_ $ maybe "" (\x -> toHtml $ x ^. P.employeeFullname) (memp ^. at (schedule ^. scheduleCreatedBy))
                td_ $ toHtml $ textShow $ utctDay $ schedule ^. scheduleCreatedOn
                td_ $ commandHtmlSubmit (Proxy :: Proxy AddEmployeesToSchedule) "Add user" "success" $
                    vHidden (schedule ^. key) :*
                    vDynamic (fmap (\e -> (e ^. P.employeeId, e ^. P.employeeFullname)) emps) :*
                    Nil
                td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveEmployeeFromSchedule) "Remove user" "success" $
                    vHidden (schedule ^. key) :*
                    vDynamic (fmap (\e -> (e ^. P.employeeId, e ^. P.employeeFullname)) emps) :*
                    Nil
                td_ $ a_ [ class_ "button", recordHref_ schedulePdfGet (schedule ^. key)] "Download"
                td_ $ toHtml $ scheduleStatus schedule
                td_ $ button_ [ class_ "alert button", type_ "button"] "Delete"
  where
    toEmployeeString [] = ""
    toEmployeeString [a] = a
    toEmployeeString [a,b] = a <> " and " <> b
    toEmployeeString (a:as) = a <> ", " <> toEmployeeString as

    memp = M.fromList
            $ catMaybes
            $ map (\emp -> case emp ^. P.employeeLogin of
                             Nothing -> Nothing
                             Just l -> Just (l, emp)) emps
    idemp = fromFoldable emps
