{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.SchedulingRequestPage where

import FUM.Types.Login
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Personio as P

schedulingRequestPage :: World -> Map Login P.Employee -> HtmlPage "scheduling-request-page"
schedulingRequestPage world memp = page_ "Scheduling Requests" (Just NavSchedulingRequest) $ do
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
            for_ (world ^. worldSchedules) $ \schedule -> do
                td_ ""
                td_ (toHtml $ schedule ^. scheduleScheduleTemplate)
                td_ (maybe "" (\x -> toHtml $ x ^. P.employeeFullname) (memp ^. at (schedule ^. scheduleCreatedBy)))
                td_ ""
                td_ ""
                td_ ""
                td_ ""
                td_ ""
                td_ ""
