{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Schedule.API where

import Futurice.IdMap            (Key)
import Futurice.Lomake
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant          (SSOUser)
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)
import Servant.Multipart         (Mem, MultipartForm)

import Futurice.App.Schedule.Command.AddEventTemplates
import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.CreateSchedule
import Futurice.App.Schedule.SchedulePdf
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.Templates

import qualified Personio as P

data Record route = Record
    { createScheduleTemplate     :: route :- SSOUser :> "schedule-template" :> ReqBody '[JSON] (AddScheduleTemplate 'Input) :> Post '[JSON] (CommandResponse ())
    , addEventTemplates          :: route :- SSOUser :> "schedule-template" :> "event-templates" :> ReqBody '[JSON] (AddEventTemplates 'Input) :> Post '[JSON] (CommandResponse ())
    , scheduleTemplateGet        :: route :- "schedule-template" :> Get '[JSON] [ScheduleTemplate]
    , schedulePdfGet             :: route :- Summary "Generate and return pdf" :> "schedule" :> Capture "templateid" (Key Schedule) :> "pdf" :> Get '[OctetStream] SchedulePdf
    } deriving (Generic)

type ScheduleAPI = ToServantApi Record

scheduleApi :: Proxy ScheduleAPI
scheduleApi = genericApi (Proxy :: Proxy Record)


data HtmlRecord route = HtmlRecord
    { createScheduleTemplateForm :: route :- SSOUser :> "schedule-template" :> MultipartForm Mem (AddScheduleTemplate 'Input) :> Post '[HTML] (HtmlPage "indexpage")
    , createNewScheduleStartForm :: route :- SSOUser :> "schedule-start" :> MultipartForm Mem CreateScheduleStart :> Post '[HTML] (HtmlPage "create-new-schedule-page")
    , createNewScheduleForm      :: route :- SSOUser :> "schedule" :> MultipartForm Mem (CreateSchedule 'Input) :> Post '[HTML] (HtmlPage "indexpage")
    , indexPageGet               :: route :- Summary "Index page" :> Get '[HTML] (HtmlPage "indexpage")
    , newSchedulePageGet         :: route :- Summary "Create new schedule page" :> "new-schedule-page" :> Get '[HTML] (HtmlPage "new-schedule-page")
    , schedulingRequestPageGet   :: route :- Summary "Scheduling requests" :> "scheduling-requests" :> Get '[HTML] (HtmlPage "scheduling-request-page")
    , personalSchedulesPageGet   :: route :- Summary "Personal schedules" :> "schedule" :> Get '[HTML] (HtmlPage "personal-schedules-page")
    , personSchedulePageGet      :: route :- Summary "Person's schedule" :> "schedule" :> Capture "employeeid" P.EmployeeId :> Capture "scheduleid" (Key Schedule) :> Get '[HTML] (HtmlPage "person-schedule")
    } deriving (Generic)

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)
