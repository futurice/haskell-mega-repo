{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursApi.API where

import Futurice.Prelude
import Futurice.Servant (SSOUser)
import Prelude ()
import Servant

import FUM.Types.Login             (Login)
import Futurice.App.HoursApi.Types

import qualified PlanMill as PM

type HoursEndpoint = "hours" :> SSOUser
    :> Description "Get hours for given interval"
    :> QueryParam' '[Required] "start-date" Day
    :> QueryParam' '[Required] "end-date" Day
    :> Get '[JSON] HoursResponse

type FutuhoursV1API =
    "projects" :> SSOUser :> Get '[JSON] [Project ReportableTask]
    :<|> "user" :> SSOUser :> Get '[JSON] User
    :<|> HoursEndpoint
    :<|> "entry" :> SSOUser :> ReqBody '[JSON] EntryUpdate :> Post '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> ReqBody '[JSON] EntryUpdate :> Put '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> Delete '[JSON] EntryUpdateResponse
    :<|> "delete-timereports" :> SSOUser :> ReqBody '[JSON] [PM.TimereportId] :> Post '[JSON] EntryUpdateResponse

type FutuhoursAPI = Get '[JSON] Text
    :<|> "api" :> "v1" :> FutuhoursV1API
    :<|> "debug" :> "users" :> Get '[JSON] [Login]

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
