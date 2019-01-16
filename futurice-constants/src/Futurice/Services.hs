{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Services (
    -- * Service Enum
    Service (..),
    serviceToText,
    serviceFromText,
    -- ** Prisms
    _Service,
    -- * PerService
    PerService (..),
    ) where

import Data.Distributive (Distributive (..))
import Data.Functor.Rep  (Representable (..), collectRep, distributeRep)
import Futurice.Aeson
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data Service
    = AvatarService
    | BadgeService
    | ChecklistService
    | ContactsService
    | ContactsApiService
    | EmailProxyService
    | FlowdockProxyService
    | FumCarbonService
    | FumService
    | FuturoomService
    | GithubProxyService
    | GithubSyncService
    | HoursService
    | HoursApiService
    | HCService
    | LibraryService
    | PersonioProxyService
    | PersonioService
    | PlanmillProxyService
    | PlanmillService
    | PlanmillSyncService
    | PowerService
    | PreferencesService
    | ProxMgmtService
    | ProxService
    | ReportsService
    | ScheduleService
    | SisosotaService
    | SmileysApiService
    | SmsProxyService
    | ThemeService
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, GhcGeneric, Lift)
  deriving anyclass (NFData, Binary, SopGeneric)
  deriving (Arbitrary) via (Sopica Service)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToHtml) via (Enumica Service)

instance TextEnum Service where
    type TextEnumNames Service =
        '["avatar"
        , "badge"
        , "checklist"
        , "contacts"
        , "contacts-api"
        , "email-proxy"
        , "flowdock-proxy"
        , "fum-carbon"
        , "fum"
        , "futuroom"
        , "github-proxy"
        , "github-sync"
        , "hours"
        , "hours-api"
        , "hc"
        , "library"
        , "personio-proxy"
        , "personio"
        , "planmill-proxy"
        , "planmill"
        , "planmill-sync"
        , "power"
        , "preferences"
        , "prox-mgmt"
        , "prox"
        , "reports"
        , "schedule"
        , "sisosota"
        , "smileys-api"
        , "sms-proxy"
        , "theme"
        ]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

serviceToText :: Service -> Text
serviceToText = enumToText

serviceFromText :: Text -> Maybe Service
serviceFromText = enumFromText

_Service :: Prism' Text Service
_Service = enumPrism

instance ToParamSchema Service where toParamSchema = enumToParamSchema
instance ToSchema Service where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- PerService
-------------------------------------------------------------------------------

data PerService a = PerService
    { perAvatar        :: a
    , perBadge         :: a
    , perChecklist     :: a
    , perContacts      :: a
    , perContactsApi   :: a
    , perEmailProxy    :: a
    , perFlowdockProxy :: a
    , perFumCarbon     :: a
    , perFum           :: a
    , perFuturoom      :: a
    , perGithubProxy   :: a
    , perGithubSync    :: a
    , perHours         :: a
    , perHoursApi      :: a
    , perHC            :: a
    , perLibrary       :: a
    , perPersonio      :: a
    , perPersonioProxy :: a
    , perPlanmill      :: a
    , perPlanmillProxy :: a
    , perPlanmillSync  :: a
    , perPower         :: a
    , perPreferences   :: a
    , perProxMgmt      :: a
    , perProx          :: a
    , perReports       :: a
    , perSchedule      :: a
    , perSisosota      :: a
    , perSmileysApi    :: a
    , perSmsProxy      :: a
    , perTheme         :: a
    }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Lift)

instance Distributive PerService where
    distribute = distributeRep
    collect    = collectRep

instance Representable PerService where
    type Rep PerService = Service

    index p AvatarService        = perAvatar p
    index p BadgeService         = perBadge p
    index p ChecklistService     = perChecklist p
    index p ContactsService      = perContacts p
    index p ContactsApiService   = perContactsApi p
    index p EmailProxyService    = perContactsApi p
    index p FlowdockProxyService = perFlowdockProxy p
    index p FumCarbonService     = perFumCarbon p
    index p FumService           = perFum p
    index p FuturoomService      = perFuturoom p
    index p GithubProxyService   = perGithubProxy p
    index p GithubSyncService    = perGithubSync p
    index p HoursService         = perHours p
    index p HoursApiService      = perHoursApi p
    index p HCService            = perHC p
    index p LibraryService       = perLibrary p
    index p PersonioProxyService = perPersonioProxy p
    index p PersonioService      = perPersonio p
    index p PlanmillProxyService = perPlanmillProxy p
    index p PlanmillService      = perPlanmill p
    index p PlanmillSyncService  = perPlanmillSync p
    index p PowerService         = perPower p
    index p PreferencesService   = perPreferences p
    index p ProxMgmtService      = perProxMgmt p
    index p ProxService          = perProx p
    index p ReportsService       = perReports p
    index p ScheduleService      = perSchedule p
    index p SisosotaService      = perSisosota p
    index p SmileysApiService    = perSmileysApi p
    index p SmsProxyService      = perSmsProxy p
    index p ThemeService         = perTheme p

    tabulate f = PerService
        { perAvatar        = f AvatarService
        , perBadge         = f BadgeService
        , perChecklist     = f ChecklistService
        , perContacts      = f ContactsService
        , perContactsApi   = f ContactsApiService
        , perEmailProxy    = f ContactsApiService
        , perFlowdockProxy = f FlowdockProxyService
        , perFumCarbon     = f FumCarbonService
        , perFum           = f FumService
        , perFuturoom      = f FuturoomService
        , perGithubProxy   = f GithubProxyService
        , perGithubSync    = f GithubSyncService
        , perHours         = f HoursService
        , perHoursApi      = f HoursApiService
        , perHC            = f HCService
        , perLibrary       = f LibraryService
        , perPersonio      = f PersonioService
        , perPersonioProxy = f PersonioProxyService
        , perPlanmill      = f PlanmillService
        , perPlanmillProxy = f PlanmillProxyService
        , perPlanmillSync  = f PlanmillSyncService
        , perPower         = f PowerService
        , perPreferences   = f PreferencesService
        , perProxMgmt      = f ProxMgmtService
        , perProx          = f ProxService
        , perReports       = f ReportsService
        , perSchedule      = f ScheduleService
        , perSisosota      = f SisosotaService
        , perSmileysApi    = f SmileysApiService
        , perSmsProxy      = f SmsProxyService
        , perTheme         = f ThemeService
        }

instance FromJSON a => FromJSON (PerService a) where
    parseJSON = withObjectDump "PerService" $ \obj ->
        for (tabulate id) $ \k -> obj .: serviceToText k
