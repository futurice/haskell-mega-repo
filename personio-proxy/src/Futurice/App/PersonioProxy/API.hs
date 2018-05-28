{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.PersonioProxy.API where

import Futurice.Prelude
import Prelude ()

import Servant

import qualified Personio

type PersonioProxyAPI =
    Get '[JSON] Text
    :<|> "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
    :<|> "employees" :> Get '[JSON] [Personio.Employee]
    :<|> Summary "Tailor made for schedule.app" :> "schedule-info" :> Get '[JSON] [Personio.ScheduleEmployee]

personioProxyApi :: Proxy PersonioProxyAPI
personioProxyApi = Proxy
