{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Schedule.Markup(
    module Futurice.Lucid.Foundation,
    linkToText,
    Nav (..),
    page_,
    futuId_) where

import Control.Lens              ((<>~))
import Futurice.Generics
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (Navigation (..), pageParamsWithJS, page_)
import Futurice.Prelude
import Prelude ()
import Servant                   (Link)

import Futurice.App.Schedule.API

import qualified Clay as C

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

    pageParams = (\x -> x & pageCss <>~ [css]) . (pageParamsWithJS $(makeRelativeToProject "schedule-app.js" >>= embedJS))

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

futuId_ :: Text -> Attribute
futuId_ = data_ "futu-id"

css :: C.Css
css = do
    C.label C.# ".error" C.? do
        C.color C.red
        "input[type=text]" C.? do
            C.borderColor C.red
        "input[type=date]" C.? do
            C.borderColor C.red
        "select" C.? do
            C.borderColor C.red
        ".select2-container--default .select2-selection--single" C.? do
            C.borderColor C.red

    C.label C.# ".pending" C.? do
        C.color C.orange
        "input[type=text]" C.?  do
            C.borderColor C.orange
        "input[type=date]" C.? do
            C.borderColor C.orange
        "select" C.? do
            C.borderColor C.orange
        ".select2-container--default .select2-selection--single" C.? do
            C.borderColor C.orange
