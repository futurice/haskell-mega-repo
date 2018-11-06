{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futuqu.API where

import Futurice.Prelude
import Prelude ()
import Futurice.Time.Month (Month)
import Servant.API
import Servant.API.Generic

import Futuqu.Rada.Accounts
import Futuqu.Rada.Capacities
import Futuqu.Rada.People
import Futuqu.Rada.Projects
import Futuqu.Rada.Tasks
import Futuqu.Rada.Timereports
import Futuqu.Ggrr.MissingHours
import Futuqu.Ggrr.HourKinds

data FutuquRoutes route = FutuquRoutes

    -- RADA : RAw DAta
    { futuquRoutePeople :: route
        :- "rada" :> "people"
        :> Summary "List of all people"
        :> Get '[JSON] [Person]
    , futuquRouteAccounts :: route
        :- "rada" :> "accounts"
        :> Summary "List of all accounts"
        :> Get '[JSON] [Account]
    , futuquRouteProjects :: route
        :- "rada" :> "projects"
        :> Summary "List of all projects"
        :> Get '[JSON] [Project]
    , futuquRouteTasks :: route
        :- "rada" :> "tasks"
        :> Summary "List of all tasks"
        :> Get '[JSON] [Task]
    , futuquRouteCapacities :: route
        :- "rada" :> "capacities"
        :> Summary "(Non-zero) daily capacities for a month"
        :> Capture "month" Month
        :> Get '[JSON] [Capacity] 
    , futuquRouteTimereports :: route
        :- "rada" :> "timereports"
        :> Summary "Timereports for a month"
        :> Capture "month" Month
        :> Get '[JSON] [Timereport] 

    -- GGRR: aGGRegate Reports
    , futuquRouteMissingHours :: route
        :- "ggrr" :> "missing-hours"
        :> Summary "Example report: missing hours"
        :> Capture "month" Month
        :> Get '[JSON] [MissingHour] 

    , futuquRouteHourKinds :: route
        :- "ggrr" :> "hours-kinds"
        :> Summary "Example report: People hours aggregated by kind: billable, non-billable etc"
        :> Capture "month" Month
        :> Get '[JSON] [HourKind] 
    }
  deriving Generic

type FutuquAPI = ToServantApi FutuquRoutes

futuquApi :: Proxy FutuquAPI
futuquApi = genericApi (Proxy :: Proxy FutuquRoutes)
