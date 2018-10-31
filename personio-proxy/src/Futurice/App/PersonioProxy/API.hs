{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.PersonioProxy.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant          (HTML)
import Prelude ()
import Servant
import Servant.Cached            (CACHED, Cached)
import Servant.Chart             (Chart, SVG)

import qualified Personio

type PersonioProxyAPI =
    Get '[HTML] (HtmlPage "index")
    :<|> "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
    :<|> "employees" :> Get '[JSON] [Personio.Employee]
    :<|> Summary "Tailor made for schedule.app" :> "schedule-info" :> Get '[JSON] [Personio.ScheduleEmployee]
    :<|> "charts" :> "employees.svg" :>          Get '[CACHED SVG] (Cached SVG (Chart "employees"))
    :<|> "charts" :> "tribe-employees.svg" :>    Get '[CACHED SVG] (Cached SVG (Chart "tribe-employees"))
    :<|> "charts" :> "career-levels.svg" :>      Get '[CACHED SVG] (Cached SVG (Chart "career-levels"))
    :<|> "charts" :> "roles-distribution.svg" :> Get '[CACHED SVG] (Cached SVG (Chart "roles-distribution"))

personioProxyApi :: Proxy PersonioProxyAPI
personioProxyApi = Proxy
