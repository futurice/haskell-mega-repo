{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Proxy.API (
    Routes (..),
    ProxyAPI,
    proxyAPI,
    proxyEndpoints,
    Endpoint,
    LenientEndpoint (..),
    ) where

import Futurice.App.Proxy.Endpoint
import Futurice.Generics
import Futurice.Prelude
import Futurice.Services
import Prelude ()
import Servant.API
import Servant.CSV.Cassava         (CSV)

-- Integrations
import FUM.Types.GroupName                    (GroupName)
import FUM.Types.Login                        (Login)
import Futurice.App.Avatar.API                (AvatarFumEndpoint)
import Futurice.App.Reports.ActiveAccounts    (ActiveAccounts)
import Futurice.App.Reports.MissingHours      (MissingHoursReport)
import Futurice.App.Reports.TimereportsByTask (TimereportsByTaskReport)
import Futurice.Signed                        (Signed)
import Servant.Binary.Tagged                  (BINARYTAGGED)

import qualified Data.Set                             as Set
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Futurice.App.Contacts.Types          as Contact
import qualified Futurice.App.SmsProxy.Types          as SMS
import qualified Futurice.FUM.MachineAPI              as FUM6
import qualified Futurice.GitHub                      as GH
                 (SomeRequest, SomeResponse)
import qualified Personio
import qualified PlanMill.Types.Query                 as PM
                 (SomeQuery, SomeResponse)

data Routes = Routes
    -- contacts
    { routeContacts :: ProxiedEndpoint 'ContactsApiService
        ("contacts.json" :> Get '[JSON] [Contact.Contact Text]) :$
            Summary "Employee data, so called contacts"
            :> "contacts" :> "contacts.json"
            :> Get '[JSON] [Contact.Contact Text]

    -- avatar
    , routeFumAvatar :: ProxiedEndpoint 'AvatarService AvatarFumEndpoint :$
          Summary "Employee avatars by FUM handle"
          :> "avatar" :> AvatarFumEndpoint

    -- Reports
    , routeMissingHours :: ProxiedEndpoint 'ReportsService
        ("missing-hours" :> Get '[JSON] MissingHoursReport)
        ("futuhours" :> "reports" :> "missinghours" :> Get '[CSV, JSON] MissingHoursReport)
    , routeHoursByTask :: ProxiedEndpoint 'ReportsService
        ("hours-by-task" :> Get '[JSON] TimereportsByTaskReport)
        ("reports" :> "hours-by-task" :> Get '[CSV, JSON] TimereportsByTaskReport)
    , routeActiveAccounts :: ProxiedEndpoint 'ReportsService
        ("tables" :> "active-accounts.json" :> Get '[JSON] ActiveAccounts)
        ("reports" :> "active-accounts" :> Get '[JSON] ActiveAccounts)

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

    , routePowerAllocations :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "allocations" :> Get '[JSON] Value)
        ("power" :> "api" :> "allocations" :> Get '[JSON] Value)

    , routePowerProjectCompetences :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "project_competences" :> Get '[JSON] Value)
        ("power" :> "api" :> "project_competences" :> Get '[JSON] Value)

    , routePowerSubcompetences :: ProxiedEndpoint 'PowerService
        ("api" :> "v2" :> "subcompetences" :> Get '[JSON] Value)
        ("power" :> "api" :> "subcompetences" :> Get '[JSON] Value)


    -- FUM
    , routeFumGroupMembers :: ProxiedEndpoint 'FumCarbonService
          FUM6.GroupMembersEndpoint
          (Summary "FUM6 group members"
              :> "fum"
              :> "groups"
              :> Capture "group-name" GroupName
              :> "employees"
              :> Get '[JSON] (Signed (Set Login)))

    -- SMS proxy
    , routeSmsSend :: ProxiedEndpoint 'SmsProxyService
          ("send" :> ReqBody '[JSON] SMS.Req :> Post '[JSON] SMS.Res)
          (Summary "Send sms"
              :> "sms" :> "send"
              :> ReqBody '[JSON] SMS.Req
              :> Post '[JSON] SMS.Res)

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

-------------------------------------------------------------------------------
-- Endpoint type
-------------------------------------------------------------------------------

-- | Endpoints in @prox@.
proxyEndpoints :: Set Endpoint
proxyEndpoints = Set.map coerce $ proxyEndpoints' (Proxy :: Proxy Routes)

newtype Endpoint = Endpoint Text
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData)

instance Enum Endpoint where
    fromEnum e = case Set.lookupIndex e proxyEndpoints of
        Nothing -> error $ "fromEnum @Endpoint: no such endpoint" ++ show e
        Just i  -> i

    toEnum i = Set.elemAt i proxyEndpoints

instance Bounded Endpoint where
    minBound = Set.findMin proxyEndpoints
    maxBound = Set.findMax proxyEndpoints

-- | No verification.
instance Textual Endpoint where
    textualToText   = coerce
    textualFromText t
        | t `Set.member` raws = Right (Endpoint t)
        | otherwise           = Left "Unknown endpoint"
      where
        raws = proxyEndpoints' (Proxy :: Proxy Routes)

deriveVia [t| ToJSON Endpoint             `Via` Textica Endpoint |]
deriveVia [t| FromJSON Endpoint           `Via` Textica Endpoint |]
deriveVia [t| ToHttpApiData Endpoint      `Via` Textica Endpoint |]
deriveVia [t| FromHttpApiData Endpoint    `Via` Textica Endpoint |]
deriveVia [t| Postgres.ToField Endpoint   `Via` Textica Endpoint |]
deriveVia [t| Postgres.FromField Endpoint `Via` Textica Endpoint |]
deriveVia [t| ToHtml Endpoint             `Via` Textica Endpoint |]

instance ToParamSchema Endpoint where toParamSchema = textualToParamSchema
instance ToSchema Endpoint where declareNamedSchema = textualDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint type
-------------------------------------------------------------------------------

-- | Parsing this always succeeds.
newtype LenientEndpoint = LenientEndpoint Text
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData)

-- | No verification.
instance Textual LenientEndpoint where
    textualToText   = coerce
    textualFromText = Right . coerce

deriveVia [t| ToJSON LenientEndpoint             `Via` Textica LenientEndpoint |]
deriveVia [t| FromJSON LenientEndpoint           `Via` Textica LenientEndpoint |]
deriveVia [t| ToHttpApiData LenientEndpoint      `Via` Textica LenientEndpoint |]
deriveVia [t| FromHttpApiData LenientEndpoint    `Via` Textica LenientEndpoint |]
deriveVia [t| Postgres.ToField LenientEndpoint   `Via` Textica LenientEndpoint |]
deriveVia [t| Postgres.FromField LenientEndpoint `Via` Textica LenientEndpoint |]
deriveVia [t| ToHtml LenientEndpoint             `Via` Textica LenientEndpoint |]

instance ToParamSchema LenientEndpoint where toParamSchema = textualToParamSchema
instance ToSchema LenientEndpoint where declareNamedSchema = textualDeclareNamedSchema
