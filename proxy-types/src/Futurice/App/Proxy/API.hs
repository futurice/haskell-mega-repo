{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
module Futurice.App.Proxy.API where

import Futurice.App.Proxy.Endpoint
import Futurice.Prelude
import Futurice.Services
import Prelude ()
import Servant.API
import Servant.CSV.Cassava         (CSV)

-- Integrations
import Servant.Binary.Tagged                (BINARYTAGGED)

import qualified Futurice.App.Contacts.Types as Contact
import qualified Personio
import qualified Futurice.GitHub             as GH (SomeRequest, SomeResponse)
import qualified Futurice.FUM.MachineAPI     as FUM6
import qualified PlanMill.Types.Query        as PM (SomeQuery, SomeResponse)
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, MissingHoursTitle)
import Futurice.App.Reports.TimereportsByTask (TimereportsByTaskReport)

data Routes = Routes
    -- contacts
    { routeContacts :: ProxiedEndpoint 'ContactsApiService
        ("contacts.json" :> Get '[JSON] [Contact.Contact Text]) :$
            Summary "Employee data, so called contacts"
            :> "contacts" :> "contacts.json"
            :> Get '[JSON] [Contact.Contact Text]

    -- Reports
    , routeMissingHours :: ProxiedEndpoint 'ReportsService
        ("missing-hours" :> Get '[JSON] (MissingHoursReport MissingHoursTitle))
        ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] (MissingHoursReport MissingHoursTitle))
    , routeHoursByTask :: ProxiedEndpoint 'ReportsService
        ("hours-by-task" :> Get '[JSON] TimereportsByTaskReport)
        ("reports" :> "hours-by-task" :> Get '[CSV, JSON] TimereportsByTaskReport)
    
    -- Power
    , routePowerBi :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "power_bi" :> QueryParam "month" Text :> QueryParam "start_month" Text :> QueryParam "end_month" Text :> QueryParam "limit" Int :> QueryParam "span" Int:> QueryParam "tribe" Text :> Get '[JSON] Value)
        ("power" :> "api" :> "power_bi" :> QueryParam "month" Text :> QueryParam "start_month" Text :> QueryParam "end_month" Text :> QueryParam "limit" Int :> QueryParam "span" Int :> QueryParam "tribe" Text :> Get '[JSON] Value)

    , routePowerCompetences :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "company_competences" :> Get '[JSON] Value)
        ("power" :> "api" :> "company_competences" :> Get '[JSON] Value)

    , routePowerPeople :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "people" :> Get '[JSON] Value)
        ("power" :> "api" :> "people" :> Get '[JSON] Value)

    , routePowerTribes :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "tribes" :> Get '[JSON] Value)
        ("power" :> "api" :> "tribes" :> Get '[JSON] Value)

    -- HAXL endpoints
    , routeFumCarbonHaxl :: ProxiedEndpoint 'FumCarbonService
        ("api" :> "haxl" :> FumCarbonEndpoint')
        (Summary "Not for integrations" :> "fum-carbon-haxl" :> FumCarbonEndpoint')
    , routePlanmillProxyHaxl :: ProxiedEndpoint 'PlanmillProxyService
        ("planmill-haxl" :> PlanmillProxyEndpoint')
        (Summary "Not for integrations" :> "planmill-haxl" :> PlanmillProxyEndpoint')
    , routeGithubProxyHaxl :: ProxiedEndpoint 'GithubProxyService
        ("github-haxl" :> GithubProxyEndpoint')
        (Summary "Not for integrations" :> "github-haxl" :> GithubProxyEndpoint')
    , routePersonioProxyHaxl :: ProxiedEndpoint 'PersonioProxyService
        ("personio-request" :> PersonioProxyEndpoint')
        (Summary "Not for integrations" :> "personio-request" :> PersonioProxyEndpoint')
    }
  deriving (Generic)

type FumCarbonEndpoint' =
    ReqBody '[JSON] [FUM6.SomeFUM6] :> Post '[JSON] [FUM6.SomeFUM6Response]
type PlanmillProxyEndpoint' =
    ReqBody '[JSON] [PM.SomeQuery] :> Post '[BINARYTAGGED] [Either Text PM.SomeResponse]
type GithubProxyEndpoint' =
    ReqBody '[JSON] [GH.SomeRequest] :> Post '[BINARYTAGGED] [Either Text GH.SomeResponse]
type PersonioProxyEndpoint' =
    ReqBody '[JSON] Personio.SomePersonioReq :>
    Post '[JSON] Personio.SomePersonioRes

-------------------------------------------------------------------------------
-- API type and its proxy
-------------------------------------------------------------------------------

type ProxyAPI  = ProxyServer Routes

proxyAPI :: Proxy ProxyAPI
proxyAPI = Proxy
