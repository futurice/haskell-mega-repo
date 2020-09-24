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
import Futurice.Servant            (GZIP)
import Futurice.Services
import Prelude ()
import Servant.API
import Servant.Cached              (CACHED, Cached)
import Servant.CSV.Cassava         (CSV)

-- Integrations
import FUM.Types.GroupName                             (GroupName)
import FUM.Types.Login                                 (Login)
import Futurice.App.Avatar.API                         (AvatarFumEndpoint)
import Futurice.App.OktaProxy.Types                    (AppResponse)
import Futurice.App.PersonioProxy.Types
       (AttritionRate, MonthlyCompensation)
import Futurice.App.Reports.ActiveAccounts             (ActiveAccounts)
import Futurice.App.Reports.Capacity                   (Capacity)
import Futurice.App.Reports.FumAbsences                (FumAbsences)
import Futurice.App.Reports.MissingHours               (MissingHoursReport)
import Futurice.App.Reports.PowerAllRevenues           (PowerAllRevenues)
import Futurice.App.Reports.ProjectMembers             (ProjectMembers)
import Futurice.App.Reports.TeamsHoursByCategoryReport
       (TeamsHoursByCategoryReport)
import Futurice.App.Reports.TimereportsByTask          (TimereportsByTaskReport)
import Futurice.App.Reports.ValueCreation              (ValueCreationReport)
import Futurice.Signed                                 (Signed)
import Servant.Binary.Tagged                           (BINARYTAGGED)

import qualified Data.Set                             as Set
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Futurice.App.Contacts.Types          as Contact
import qualified Futurice.App.Library.Types           as Library
import qualified Futurice.App.SmsProxy.Types          as SMS
import qualified Futurice.FUM.MachineAPI              as FUM6
import qualified Futurice.GitHub                      as GH
                 (SomeRequest, SomeResponse)
import qualified Personio
import qualified PlanMill                             as PM
import qualified PlanMill.Types.Query                 as PM
                 (SomeQuery, SomeResponse)

type Futuqu endpoint = ProxiedEndpoint 'ReportsService
    ("futuqu" :> endpoint)
    ("futuqu" :> endpoint)

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
    , routeProjectMembers :: ProxiedEndpoint 'ReportsService
        ("project" :> "members" :> Get '[JSON] [ProjectMembers])
        ("reports" :> "project" :> "members" :> Get '[JSON] [ProjectMembers])
    , routeAllRevenue :: ProxiedEndpoint 'ReportsService
        ("power" :> "all-revenue" :> QueryParam "month" Month :> Get '[JSON] PowerAllRevenues)
        ("reports" :> "all-revenues" :>  QueryParam "month" Month :> Get '[JSON] PowerAllRevenues)
    , routeValueCreation :: ProxiedEndpoint 'ReportsService
        ("report" :> "value-creation" :> QueryParam "year" Integer :> Get '[JSON] ValueCreationReport)
        ("reports" :> "value-creation" :>  QueryParam "year" Integer :> Get '[JSON] ValueCreationReport)
    , routeTeamsHours :: ProxiedEndpoint 'ReportsService
        ("report" :> "teams-hours-by-category" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] TeamsHoursByCategoryReport)
        ("reports" :> "teams-hours-by-category" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] TeamsHoursByCategoryReport)

    , routeFumCapacity :: ProxiedEndpoint 'ReportsService
        ("report" :> "capacity" :> Capture "login" Login :> Capture "month" Month :> Get '[JSON] [Capacity])
        ("reports" :> "capacity" :> Capture "login" Login :> Capture "month" Month :> Get '[JSON] [Capacity])
    , routeFumAbsence :: ProxiedEndpoint 'ReportsService
        ("report" :> "absences" :> Capture "login" Login :> Capture "month" Month :> Get '[JSON] FumAbsences)
        ("reports" :> "absences" :> Capture "login" Login :> Capture "month" Month :> Get '[JSON] FumAbsences)

    -- Futuqu: we could real types, but this way is simpler.
    , routeFutuquPeople       :: Futuqu ("rada" :> "people"                                 :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquPeopleSimple :: Futuqu ("rada" :> "people" :> "simple"                     :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquAccount      :: Futuqu ("rada" :> "accounts"                               :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquProjects     :: Futuqu ("rada" :> "projects"
        :> QueryParams "account" PM.AccountId
        :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquTasks        :: Futuqu ("rada" :> "tasks"
        :> QueryParams "account" PM.AccountId
        :> QueryParams "project" PM.ProjectId
        :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquCapacities   :: Futuqu ("rada" :> "capacities"    :> Capture "month" Month :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquTimereports  :: Futuqu ("rada" :> "timereports"   :> Capture "month" Month :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquMissingHours :: Futuqu ("ggrr" :> "missing-hours" :> Capture "month" Month :> Get '[CACHED CSV] (Cached CSV [Text]))
    , routeFutuquHoursKinds   :: Futuqu ("ggrr" :> "hours-kinds"   :> Capture "month" Month :> Get '[CACHED CSV] (Cached CSV [Text]))

    -- streaming futuqu
    , routeFutuquTimereportsStream :: Futuqu ("strm" :> "timereports.csv.gz" :> QueryParam "since-month" Month :> StreamGet NoFraming (CACHED (GZIP CSV)) (SourceIO (Cached (GZIP CSV) Text)))
    , routeFutuquCapacitiesStream  :: Futuqu ("strm" :> "capacities.csv.gz"  :> QueryParam "since-month" Month :> StreamGet NoFraming (CACHED (GZIP CSV)) (SourceIO (Cached (GZIP CSV) Text)))

    -- Power v3 (2020)
    , routePowerV3PersonCompetences :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "timeline" :> "personskill/" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "person_competences" :> Get '[JSON] Value)

    , routePowerV3ProjectCompetences :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "timeline" :> "projectskill/" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "project_competences" :> Get '[JSON] Value)

    , routePowerV3Competences :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "skill/" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "competences" :> Get '[JSON] Value)

    , routePowerV3People :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "person" :> "" :>  QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "people" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routePowerV3PeopleAlternative :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "person" :> "" :>  QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "person" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routePowerV3Tribes :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "tribe/" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "tribes" :> Get '[JSON] Value)

    , routePowerV3TribesMonthlyForecast :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "tribe" :> "monthly_forecast/" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "tribes" :> "monthly" :> "forecast" :> Get '[JSON] Value)

    , routePowerV3Allocations :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "allocation" :> "" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "allocations" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routePowerV3AllocationsHistory :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "allocation" :> "history" :> "" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "allocations" :> "history" :> Get '[JSON] Value)

    , routePowerV3Projects :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "project" :> "" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "projects" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routePowerV3Customers :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "customer" :> "" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "customers" :> Get '[JSON] Value)

    , routePowerV3PowerToPlanMillProject :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "powerprojecttoplanmillproject" :> "" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "powerprojecttoplanmillproject" :> Get '[JSON] Value)

    , routePowerV3PersonSkillExperience :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "personskillexperience" :> "" :> Get '[JSON] Value)
        ("power" :> "api" :> "v3" :> "personskillexperience" :> Get '[JSON] Value)

    -- Power dev endpoints
    , routeDevPowerPeople :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "person" :> "" :>  QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("dev" :> "power" :> "uiapi" :> "person" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routeDevPowerAllocations :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "allocation" :> "" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("dev" :> "power" :> "uiapi" :> "allocation" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routeDevPowerCustomers :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "customer" :> "" :> Get '[JSON] Value)
        ("dev" :> "power" :> "uiapi" :> "customer" :> Get '[JSON] Value)

    , routeDevPowerProjects :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "project" :> "" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)
        ("dev" :> "power" :> "uiapi" :> "project" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[JSON] Value)

    , routeDevPowerProjectMapping :: ProxiedEndpoint 'PowerService
        ("uiapi" :> "powerprojecttoplanmillproject" :> "" :> Get '[JSON] Value)
        ("dev" :> "power" :> "api" :> "v3" :> "powerprojecttoplanmillproject" :> Get '[JSON] Value)

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

    -- Library endpoints
    , routeLibraryBooksGet :: ProxiedEndpoint 'LibraryService
        ("book" :> Get '[JSON] [Library.BookInformationResponse])
        (Summary "Get all books" :> "library" :> "book" :> Get '[JSON] [Library.BookInformationResponse])
    , routeLibraryLoansGet :: ProxiedEndpoint 'LibraryService
        ("loan" :> Get '[JSON] [Library.LoanResponse])
        (Summary "Get all loans" :> "library" :> "loan" :> Get '[JSON] [Library.LoanResponse])
    , routeLibraryBorrowBookPost :: ProxiedEndpoint 'LibraryService
        ("book" :> "user" :> "borrow" :> ReqBody '[JSON] Library.BorrowRequestWithUser :> Post '[JSON] Library.LoanResponse)
        (Summary "Borrow a book for user" :> "library" :> "book" :> "borrow" :> ReqBody '[JSON] Library.BorrowRequestWithUser :> Post '[JSON] Library.LoanResponse)
    , routeLibraryReturnPost :: ProxiedEndpoint 'LibraryService
        ("return" :> Capture "id" Library.LoanId :> Post '[JSON] Bool)
        (Summary "Return a loan" :> "library" :> "return" :> Capture "id" Library.LoanId :> Post '[JSON] Bool)

    -- Peakon endpoints
    , routePeakonEngagementOverview :: ProxiedEndpoint 'ReportsService
        ("peakon" :> "engagement" :> "overview" :> Get '[JSON] Value)
        ("peakon" :> "engagement" :> "overview" :> Get '[JSON] Value)
    , routePeakonEngagementDrivers :: ProxiedEndpoint 'ReportsService
        ("peakon" :> "engagement" :> "drivers"  :> Get '[JSON] Value)
        ("peakon" :> "engagement" :> "drivers"  :> Get '[JSON] Value)
    , routePeakonSegments :: ProxiedEndpoint 'ReportsService
        ("peakon" :> "segments" :> Get '[JSON] Value)
        ("peakon" :> "segments" :> Get '[JSON] Value)

    -- Personio endpoints
    , routeAttritionRate :: ProxiedEndpoint 'PersonioProxyService
        ("stats" :> "attrition-rate" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] AttritionRate)
        ("personio" :> "attrition-rate" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] AttritionRate)
    , routeAverageTargetMonthlyCompensation :: ProxiedEndpoint 'PersonioProxyService
        ("stats" :> "average-target-monthly-compensation" :> Get '[JSON] MonthlyCompensation)
        ("personio" :> "average-target-monthly-compensation" :> Get '[JSON] MonthlyCompensation)
    , routeUsername :: ProxiedEndpoint 'PersonioProxyService
        ("username" :> Capture "employee-name" Text :> Get '[PlainText] Login)
        ("personio" :> "username" :> Capture "employee-name" Text :> Get '[PlainText] Login)

    -- Okta endpoints
    , routeGroupMembers :: ProxiedEndpoint 'OktaProxyService
        ("group" :> "members" :> ReqBody '[JSON] Text :> Post '[JSON] [Login])
        ("okta" :> "group" :> "members" :> ReqBody '[JSON] Text :> Post '[JSON] [Login])
    , routeEmployeeApplications :: ProxiedEndpoint 'OktaProxyService
        ("applications" :> QueryParam' '[Required] "employeeId" Personio.EmployeeId :> Get '[JSON] (Set AppResponse))
        ("okta" :> "applications" :> QueryParam' '[Required] "employeeId" Personio.EmployeeId :> Get '[JSON] (Set AppResponse))
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
