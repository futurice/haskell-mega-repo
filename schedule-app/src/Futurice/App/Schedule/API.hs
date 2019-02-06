{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Schedule.API where

import Futurice.Lomake
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant          (SSOUser)
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)

import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.Definition

data Record route = Record
    { indexPageGet :: route :- Summary "Index page" :> Get '[HTML] (HtmlPage "indexpage")
    , createScheduleTemplate :: route :- SSOUser :> "schedule-template" :> ReqBody '[JSON] (AddScheduleTemplate 'Input) :> Post '[JSON] (CommandResponse ())
    } deriving (Generic)

type ScheduleAPI = ToServantApi Record

scheduleApi :: Proxy ScheduleAPI
scheduleApi = genericApi (Proxy :: Proxy Record)
