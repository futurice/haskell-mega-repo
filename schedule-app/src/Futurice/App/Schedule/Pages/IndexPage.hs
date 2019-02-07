{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.IndexPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Config
import Futurice.App.Schedule.Ctx
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types
import Futurice.App.Schedule.World

indexPage :: Ctx -> World -> HtmlPage "indexpage"
indexPage ctx world = page_ "Home" (Just NavHome) $ do
    let cfg = ctxConfig ctx
    div_ [] $
        sortableTable_ $ do
            thead_ $ do
                th_ $ "Template"
                th_ $ "TimeZone"
                th_ $ "Calendar"
                th_ $ "Actions"
            tbody_ $ do
                for_ (world ^. worldScheduleTemplates) $ \s -> tr_ $ do
                    td_ $ toHtml $ s ^. scheduleName
                    td_ $ toHtml $ show $ s ^. scheduleTimezone
                    td_ $ toHtml $ calendarToText $ s ^. scheduleCalendar
                    td_ $ span_ ""
                tr_ $ do
                    td_ $ input_ [class_ "text"]
                    td_ $ span_ ""
                    td_ $ select_ [] $ do
                        for_ (cfgCalendar cfg) $ \cal -> do
                            optionSelected_ False [ value_ $ calendarToText cal ] $ toHtml $ calendarToText cal
    form_ [recordAction_ createScheduleTemplateForm, method_ "Post", enctype_ "multipart/form-data"] $ do
        input_ [ name_ "name", type_ "text"]
        select_ [ name_ "calendar"] $ do
            for_ (cfgCalendar cfg) $ \cal -> do
                optionSelected_ False [ value_ $ calendarToText cal ] $ toHtml $ calendarToText cal
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
