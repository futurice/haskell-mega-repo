{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Schedule.Markup(
    module Futurice.Lucid.Foundation,
    linkToText,
    Nav (..),
    page_,
    futuId_) where

import Futurice.Generics
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithJS, page_)
import Futurice.Prelude
import Prelude ()
import Servant                   (Link)

import Futurice.App.Schedule.API

data Nav = NavHome
         | NavNewSchedule
         | NavSchedulingRequest
         | NavPersonalSchedules
    deriving (Eq, Enum, Bounded, Ord)

instance Navigation Nav where
    serviceTitle _ = "Schedule"

    navLink NavHome              = (recordHref_ indexPageGet, "Schedule Home")
    navLink NavNewSchedule       = (recordHref_ newSchedulePageGet, "Create New Schedule")
    navLink NavSchedulingRequest = (recordHref_ schedulingRequestPageGet, "Scheduling Requests")
    navLink NavPersonalSchedules = (recordHref_ personalSchedulesPageGet, "Personal Schedules")

    pageParams = pageParamsWithJS $(makeRelativeToProject "schedule-app.js" >>= embedJS)

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

futuId_ :: Text -> Attribute
futuId_ = data_ "futu-id"