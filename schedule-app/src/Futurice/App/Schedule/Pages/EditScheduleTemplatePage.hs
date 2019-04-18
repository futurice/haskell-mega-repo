{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.EditScheduleTemplatePage where

import Futurice.IdMap   (Key, key)
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.AddEventTemplate
import Futurice.App.Schedule.Command.EditScheduleTemplate
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Data.Map as M
import qualified Personio as P

editScheduleTemplatePage :: World -> [P.Employee] -> Key ScheduleTemplate -> HtmlPage "edit-scheduletemplate-page"
editScheduleTemplatePage w _emps sid = page_ "Edit template" (Just NavHome) $ do
    fullRow_ $ do
        div_ [ id_ "edit-template-accordion"] $ do
            ul_ [ class_ "accordion", data_ "accordion" "", data_ "multi-expand" "true", data_ "allow-all-closed" "true"] $ do
                for_ (w ^. worldScheduleTemplates . at sid) $ \scheduleTemplate -> do
                    for_ (M.keys $ eventTemplateMap (toList $ scheduleTemplate ^. scheduleEventTemplates)) $ \eid -> do
                        h2_ $ weekdays !! (fromIntegral $ getOffset eid)
                        for_ ((eventTemplateMap (toList $ scheduleTemplate ^. scheduleEventTemplates)) ^. ix eid) $ \eventTemplate -> do
                            li_ [ class_ "accordion-item", data_ "accordion-item" ""] $ do
                                let _i = 1
                                a_ [ class_ "accordion-title", href_ "#"] $ toHtml $ eventTemplate ^. etSummary
                                div_ [class_ "accordion-content", data_ "tab-content" ""] $ do
                                    commandHtmlSubmit (Proxy :: Proxy EditEventTemplate) "Edit" "success" $
                                        vHidden (scheduleTemplate ^. key) :*
                                        vHidden (eventTemplate ^. key) :*
                                        vNothing :*
                                        vNothing :*
                                        vNothing :*
                                        vNothing :*
                                        vNothing :*
                                        vJust True :*
                                        vJust True :*
                                        vNothing :*
                                        Nil
    --                                 table_ $ do
    --                                     tr_ $ do
    --                                         th_ "Summary:"
    --                                         td_ $ input_ [ type_ "text", name_ ("summary-" <> textShow i) , value_ (eventTemplate ^. etSummary)]
    --                                     tr_ $ do
    --                                         th_ "Description:"
    --                                         td_ $ textarea_ [ name_ ("description-" <> textShow i) ] $ toHtml (eventTemplate ^. etDescription)
    --                                     tr_ $ do
    --                                         th_ "Locations:"
    --                                         td_ $ ""
    --                                     tr_ $ do
    --                                         th_ "People to invite:"
    --                                         td_ $ select_ [ futuId_ "schedule-employees", multiple_ "multiple", name_ ("employees-" <> textShow i)] $ do
    --                                             for_ emps $ \e -> do
    --                                                 optionSelected_ False [ value_ (textShow $ e ^. P.employeeId)] $ toHtml (e ^. P.employeeFullname)
    -- fullRow_ $ do
        h3_ "Add a new template"
        commandHtmlSubmit (Proxy :: Proxy AddEventTemplate) "Add" "success" $
            vHidden sid :*
            vNothing :*
            vNothing :*
            Nil
  where
    eventTemplateMap :: [EventTemplate] -> M.Map DayOffset [EventTemplate]
    eventTemplateMap eventTemplates = M.fromListWith (<>) $ fmap (\e -> (either id (const (DayOffset 0)) (e ^. etTimeOffset), [e])) eventTemplates

    weekdays = cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
