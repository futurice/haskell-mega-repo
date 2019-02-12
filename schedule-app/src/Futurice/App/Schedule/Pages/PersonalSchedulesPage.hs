{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.PersonalSchedulesPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.World

personalSchedulesPage :: World -> HtmlPage "personal-schedules-page"
personalSchedulesPage world = page_ "Personal Schedules" (Just NavPersonalSchedules) $ do
    p_ ""
