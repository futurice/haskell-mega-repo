{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Futuqu.API (
    FutuquAPI,
    futuquApi,
    FutuquRoutes (..),
    FutuquGet,
    ) where

import Futurice.Prelude
import Futurice.Time.Month (Month)
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.CSV.Cassava (CSV)

import qualified PlanMill as PM

import Futuqu.Ggrr.HourKinds
import Futuqu.Ggrr.MissingHours
import Futuqu.Rada.Accounts
import Futuqu.Rada.Capacities
import Futuqu.Rada.People
import Futuqu.Rada.Projects
import Futuqu.Rada.Tasks
import Futuqu.Rada.Timereports
import Futuqu.Servant.CSV

data FutuquRoutes route = FutuquRoutes

    -- RADA : RAw DAta
    { futuquRoutePeople :: route
        :- "rada" :> "people"
        :> Summary "List of all people"
        :> FutuquGet [Person]
    , futuquRouteAccounts :: route
        :- "rada" :> "accounts"
        :> Summary "List of all accounts"
        :> FutuquGet [Account]
    , futuquRouteProjects :: route
        :- "rada" :> "projects"
        :> Summary "List of all projects"
        :> QueryParams "account" PM.AccountId
        :> FutuquGet [Project]
    , futuquRouteTasks :: route
        :- "rada" :> "tasks"
        :> Summary "List of all tasks"
        :> QueryParams "account" PM.AccountId
        :> QueryParams "project" PM.ProjectId
        :> FutuquGet [Task]
    , futuquRouteCapacities :: route
        :- "rada" :> "capacities"
        :> Summary "(Non-zero) daily capacities for a month"
        :> Capture "month" Month
        :> FutuquGet [Capacity]
    , futuquRouteTimereports :: route
        :- "rada" :> "timereports"
        :> Summary "Timereports for a month"
        :> Capture "month" Month
        :> FutuquGet [Timereport]

    -- STReaMing
    , futuquRouteTimereportsStream :: route
        :- "strm" :> "timereports.csv"
        :> Summary "All timereports"
        :> QueryParam "since-month" Month
        -- TODO: technically we should be fine with SourceIO Timereport;
        -- but then we'll need to define own content type
        -- yet we need one to get header.
        :> StreamGet (CSVFraming Timereport) CSVStreaming (SourceIO Timereport)

    -- GGRR: aGGRegate Reports
    , futuquRouteMissingHours :: route
        :- "ggrr" :> "missing-hours"
        :> Summary "Example report: missing hours"
        :> Capture "month" Month
        :> FutuquGet [MissingHour]

    , futuquRouteHourKinds :: route
        :- "ggrr" :> "hours-kinds"
        :> Summary "Example report: People hours aggregated by kind: billable, non-billable etc"
        :> Capture "month" Month
        :> FutuquGet [HourKind]
    }
  deriving Generic

type FutuquGet res = Get '[JSON, CSV] res

type FutuquAPI = ToServantApi FutuquRoutes

futuquApi :: Proxy FutuquAPI
futuquApi = genericApi (Proxy :: Proxy FutuquRoutes)
