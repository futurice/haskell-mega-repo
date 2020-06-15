{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.PersonioProxy.API where

import Codec.Picture             (DynamicImage)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant          (HTML)
import Prelude ()
import Servant
import Servant.Cached            (CACHED, Cached)
import Servant.Chart             (Chart, SVG)
import Servant.JuicyPixels       (PNG)

import Futurice.App.PersonioProxy.Types

import qualified Personio

type PersonioProxyAPI =
    Get '[HTML] (HtmlPage "index")
    :<|> "stats" :> Get '[HTML] (HtmlPage "stats")
    :<|> "stats" :> "attrition-rate" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] AttritionRate
    :<|> "stats" :> "average-target-monthly-compensation" :> Get '[JSON] MonthlyCompensation
    :<|> "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
--    :<|> "employees" :> Get '[JSON] [Personio.Employee]
    :<|> "employee-picture" :> Capture "employee-id" Personio.EmployeeId :> Get '[PNG] DynamicImage
    :<|> Summary "Tailor made for schedule.app" :> "schedule-info" :> Get '[JSON] [Personio.ScheduleEmployee]
    :<|> Summary "Tailor made for inventory.app" :> "inventory" :> Get '[JSON] [Personio.InventoryEmployee]
    :<|> "charts" :> "employees.svg" :>          Get '[CACHED SVG] (Cached SVG (Chart "employees"))
    :<|> "charts" :> "tribe-employees.svg" :>    Get '[CACHED SVG] (Cached SVG (Chart "tribe-employees"))
    :<|> "charts" :> "career-levels.svg" :>      Get '[CACHED SVG] (Cached SVG (Chart "career-levels"))
    :<|> "charts" :> "roles-distribution.svg" :> Get '[CACHED SVG] (Cached SVG (Chart "roles-distribution"))

personioProxyApi :: Proxy PersonioProxyAPI
personioProxyApi = Proxy
