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
import Futurice.App.Schedule.Types.TimeZoneInfo
import Futurice.App.Schedule.World

import qualified Data.Text as T

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
                    td_ $ toHtml $ timeZoneToText $ s ^. scheduleTimezone
                    td_ $ toHtml $ calendarToText $ s ^. scheduleCalendar
                    td_ $ span_ ""
    fullRow_ $ form_ [recordAction_ createScheduleTemplateForm, method_ "Post", enctype_ "multipart/form-data"] $ do
        label_ "Create new template"
        large_ 3 $ input_ [ name_ "name", type_ "text"]
        large_ 3 $ select_ [ name_ "timezone"] $ do
            for_ officeTimeLabelsStrings $ \tz -> do
                optionSelected_ False [ value_ $ T.pack tz ] $ toHtml $ tz
        large_ 3 $ select_ [ name_ "calendar"] $ do
            for_ (cfgCalendar cfg) $ \cal -> do
                optionSelected_ False [ value_ $ calendarToText cal ] $ toHtml $ calendarToText cal
        button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Create"
