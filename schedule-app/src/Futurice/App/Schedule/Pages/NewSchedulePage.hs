{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.NewSchedulePage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Personio as P

newSchedulePage :: [P.Employee] -> World -> HtmlPage "new-schedule-page"
newSchedulePage emps world = page_ "New Schedule" (Just NavNewSchedule) $ do
    h2_ "Create New Schedule"
    form_ [recordAction_ createNewScheduleStartForm, method_ "Post", enctype_ "multipart/form-data"] $ do
        label_ $ do
            toHtml ("From template" :: Text)
            select_ [ name_ "template"] $ do
                for_ (world ^. worldScheduleTemplates) $ \template -> do
                    optionSelected_ False [ value_ (template ^. scheduleName)] $ toHtml (template ^. scheduleName)
        label_ $ do
            toHtml ("Start date" :: Text)
            input_ [type_ "date", name_ "start-date"]
        label_ $ do
            toHtml ("For Employees" :: Text)
            select_ [ futuId_ "schedule-employees", multiple_ "multiple", name_ "employees"] $ do
                for_ emps $ \e -> do
                    optionSelected_ False [ value_ (showId $ e ^. P.employeeId)] $ toHtml (e ^. P.employeeFullname)
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Submit"
    where
      showId (P.EmployeeId i) = textShow i
