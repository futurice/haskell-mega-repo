{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Schedule.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)

data Record route = Record
    { indexPageGet :: route :- Summary "Index page" :> Get '[HTML] (HtmlPage "indexpage")
    } deriving (Generic)

type ScheduleAPI = ToServantApi Record

scheduleApi :: Proxy ScheduleAPI
scheduleApi = genericApi (Proxy :: Proxy Record)
