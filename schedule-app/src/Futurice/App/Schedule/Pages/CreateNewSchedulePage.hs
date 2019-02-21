{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.CreateNewSchedulePage where

import Futurice.IdMap   (toMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Command.CreateSchedule
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Personio as P

createNewSchedulePage :: CreateScheduleStart -> [P.Employee] -> World -> HtmlPage "create-new-schedule-page"
createNewSchedulePage (CreateScheduleStart name date scheduleEmps) emps world = page_ "New Schedule" (Just NavNewSchedule) $ do
    form_ [recordAction_ createNewScheduleForm, method_ "Post", enctype_ "multipart/form-data"] $ for_ (world ^. worldScheduleTemplates . at name) $ \template -> do
        input_ [ type_ "hidden", name_ "schedule-template", value_ name ]
        ifor_ (M.elems . toMap $ template  ^. scheduleEventTemplates) $ \i eventTemplate -> do
            input_ [ type_ "hidden", name_ ("event-template-" <> textShow i), value_ "" ] --TODO fill right
            section_ $ table_ $ do
                tr_ $ do
                    th_ "Summary:"
                    td_ $ input_ [ type_ "text", name_ ("summary-" <> textShow i) , value_ (eventTemplate ^. etSummary)]
                tr_ $ do
                    th_ "Description:"
                    td_ $ textarea_ [ name_ ("description-" <> textShow i) ] $ toHtml (eventTemplate ^. etDescription)
                tr_ $ do
                    th_ "Locations:"
                    td_ $ ""
                tr_ $ do
                    th_ "Date:"
                    td_ $ input_ [ type_ "date", name_ ("start-date-" <> textShow i), value_ (textShow date)] -- TODO: lisää offset
                tr_ $ do
                    th_ "From:"
                    td_ $ do -- TODO: fix layout
                        large_ 3 $ input_ [ type_ "time", name_ ("from-" <> textShow i), value_ (textShow $ eventTemplate ^. etStartTime)]
                        large_ 1 $ toHtml (" to " :: Text)
                        large_ 3 $ input_ [ type_ "time", name_ ("to-" <> textShow i), value_ (textShow $ eventTemplate ^. etEndTime)]
                tr_ $ do
                    th_ "People to invite:"
                    td_ $ select_ [ futuId_ "schedule-employees", multiple_ "multiple", name_ ("employees-" <> textShow i)] $ do
                        for_ emps $ \e -> do
                            optionSelected_ (S.member (e ^. P.employeeId) (S.fromList scheduleEmps)) [ value_ (showId $ e ^. P.employeeId)] $ toHtml (e ^. P.employeeFullname)
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Submit"
  where
      showId (P.EmployeeId i) = textShow i
