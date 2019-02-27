{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.PersonSchedule where

import Futurice.Email            (emailToText)
import Futurice.IdMap            (Key)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Personio as P

personSchedule :: World -> Key Schedule -> P.Employee -> HtmlPage "person-schedule"
personSchedule world sid emp = page_ "Schedule" (Just NavPersonalSchedules) $ do
    h2_ $ "Schedule for " <> emp ^. P.employeeFullname <> " (" <> maybe "" emailToText (emp ^. P.employeeEmail) <> ")"
    ul_ $ do
        for_ (world ^. worldSchedules . at sid) $ \s -> do
            for_ (s ^. scheduleEvents) $ \e -> do
                li_ $ div_ $ do
                    toHtml $ e ^. eventSummary
                    br_ []
                    toHtml $ e ^. eventDescription
