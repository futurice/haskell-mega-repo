{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.NewSchedulePage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types
import Futurice.App.Schedule.World

newSchedulePage :: World -> HtmlPage "new-schedule-page"
newSchedulePage world = page_ "New Schedule" (Just NavNewSchedule) $ do
    h2_ "Create New Schedule"
    form_ [method_ "Post", enctype_ "multipart/form-data"] $ do --TODO: add recordaction
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
