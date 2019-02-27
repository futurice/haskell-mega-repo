{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Schedule.Pages.PersonalSchedulesPage where

import Data.Ord                  (Down (..))
import Futurice.IdMap            (IdMap, key, toMap)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Data.Set as S
import qualified Personio as P

personalSchedulesPage :: World -> IdMap P.Employee -> HtmlPage "personal-schedules-page"
personalSchedulesPage world emap = page_ "Personal Schedules" (Just NavPersonalSchedules) $ do
    h2_ "Personal Schedules created by the system"
    sortableTable_ $ do
        thead_ $ do
            th_ "For"
            th_ "Template"
            th_ "Created"
        tbody_ $ do
            for_ personSchedules $ \(pid, s) -> tr_ $ do
                td_ $ a_ [ recordHref_ personSchedulePageGet pid (s ^. key)] (toHtml $ emap ^. ix pid . P.employeeFullname)
                td_ (toHtml $ s ^. scheduleTemplateName)
                td_ (toHtml $ textShow $ utctDay $ s ^. scheduleCreatedOn)
  where
    personSchedules :: [(P.EmployeeId, Schedule)]
    personSchedules = let schedules = world ^. worldSchedules
                          schedules'' = concat $ (\s -> fmap ( ,s) (S.toList $ peopleOnSchedule s)) <$> toMap schedules
                      in sortOn (Down . (^. scheduleCreatedOn) . snd) schedules''
