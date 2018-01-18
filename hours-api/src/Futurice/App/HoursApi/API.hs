{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

type FutuhoursV1API =
    "projects" :> SSOUser :> Get '[JSON] [Project ReportableTask]
    :<|> "user" :> SSOUser :> Get '[JSON] User
    :<|> "hours" :> SSOUser :> QueryParam "start-date" Day :> QueryParam "end-date" Day :> Get '[JSON] (HoursResponse)
    :<|> "entry" :> SSOUser :> ReqBody '[JSON] EntryUpdate :> Post '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> ReqBody '[JSON] EntryUpdate :> Put '[JSON] EntryUpdateResponse
    :<|> "entry" :> SSOUser :> Capture "id" PM.TimereportId :> Delete '[JSON] EntryUpdateResponse
    :<|> "settings" :> SSOUser :> Get '[JSON] SettingsResponse
    :<|> "settings" :> SSOUser :> ReqBody '[JSON] (SettingsResponse,SettingsResponse) :> Post '[JSON] SettingsUpdateResponse

type FutuhoursAPI = Get '[JSON] Text
    :<|> "api" :> "v1" :> FutuhoursV1API
    :<|> "debug" :> "users" :> Get '[JSON] [Login]

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
