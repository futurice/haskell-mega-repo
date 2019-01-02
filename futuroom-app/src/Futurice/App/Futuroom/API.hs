{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Futuroom.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)

import Futurice.App.Futuroom.Types

----------------------------
-- Machine API
----------------------------

data Record route = Record
    { reservationsGet :: route :- "reservation" :> QueryParam "week" WeekNumber :> QueryParam "year" Year :> Get '[JSON] [Reservation]
    } deriving Generic

type FuturoomAPI = ToServantApi Record

futuroomApi :: Proxy FuturoomAPI
futuroomApi = genericApi (Proxy :: Proxy Record)

----------------------------
-- Page API
----------------------------

data HtmlRecord route = HtmlRecord
    { indexPageGet :: route :- QueryParam "date" Day :> Get '[HTML] (HtmlPage "indexpage")
    } deriving Generic

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)
