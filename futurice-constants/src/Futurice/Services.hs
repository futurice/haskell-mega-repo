{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
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
    | ChecklistService
    | ContactsService
    | ContactsApiService
    | FumCarbonService
    | FumService
    | GithubProxyService
    | GithubSyncService
    | HoursService
    | HoursApiService
    | HCService
    | PersonioService
    | PersonioProxyService
    | PlanmillService
    | PlanmillProxyService
    | PlanmillSyncService
    | PowerService
    | ProxMgmtService
    | ProxService
    | ReportsService
    | ThemeService
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic, Lift)
  deriving anyclass (NFData, Binary)

-- makePrism ''Service
deriveGeneric ''Service

instance TextEnum Service where
    type TextEnumNames Service =
        '["avatar"
        , "checklist"
        , "contacts"
        , "contacts-api"
        , "fum-carbon"
        , "fum"
        , "github-proxy"
        , "github-sync"
        , "hours"
        , "hours-api"
        , "hc"
        , "personio"
        , "personio-proxy"
        , "planmill"
        , "planmill-proxy"
        , "planmill-sync"
        , "power"
        , "prox-mgmt"
        , "prox"
        , "reports"
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

deriveVia [t| Arbitrary Service       `Via` Sopica Service  |]
deriveVia [t| ToJSON Service          `Via` Enumica Service |]
deriveVia [t| FromJSON Service        `Via` Enumica Service |]
deriveVia [t| ToHttpApiData Service   `Via` Enumica Service |]
deriveVia [t| FromHttpApiData Service `Via` Enumica Service |]
-- deriveVia [t| Csv.ToField Service     `Via` Enumica Service |]
-- deriveVia [t| Csv.FromField Service   `Via` Enumica Service |]
deriveVia [t| ToHtml Service          `Via` Enumica Service |]

instance ToParamSchema Service where toParamSchema = enumToParamSchema
instance ToSchema Service where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- PerService
-------------------------------------------------------------------------------

data PerService a = PerService
    { perAvatar        :: a
    , perChecklist     :: a
    , perContacts      :: a
    , perContactsApi   :: a
    , perFumCarbon     :: a
    , perFum           :: a
    , perGithubProxy   :: a
    , perGithubSync    :: a
    , perHours         :: a
    , perHoursApi      :: a
    , perHC            :: a
    , perPersonio      :: a
    , perPersonioProxy :: a
    , perPlanmill      :: a
    , perPlanmillProxy :: a
    , perPlanmillSync  :: a
    , perPower         :: a
    , perProxMgmt      :: a
    , perProx          :: a
    , perReports       :: a
    , perTheme         :: a
    }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Lift)

instance Distributive PerService where
    distribute = distributeRep
    collect    = collectRep

instance Representable PerService where
    type Rep PerService = Service

    index p AvatarService        = perAvatar p
    index p ChecklistService     = perChecklist p
    index p ContactsService      = perContacts p
    index p ContactsApiService   = perContactsApi p
    index p FumCarbonService     = perFumCarbon p
    index p FumService           = perFum p
    index p GithubProxyService   = perGithubProxy p
    index p GithubSyncService    = perGithubSync p
    index p HoursService         = perHours p
    index p HoursApiService      = perHoursApi p
    index p HCService            = perHC p
    index p PersonioProxyService = perPersonioProxy p
    index p PersonioService      = perPersonio p
    index p PlanmillProxyService = perPlanmillProxy p
    index p PlanmillService      = perPlanmill p
    index p PlanmillSyncService  = perPlanmillSync p

    index p PowerService         = perPower p
    index p ProxMgmtService      = perProxMgmt p
    index p ProxService          = perProx p
    index p ReportsService       = perReports p
    index p ThemeService         = perTheme p

    tabulate f = PerService
        {  perAvatar        = f AvatarService
        ,  perChecklist     = f ChecklistService
        ,  perContacts      = f ContactsService
        ,  perContactsApi   = f ContactsApiService
        ,  perFumCarbon     = f FumCarbonService
        ,  perFum           = f FumService
        ,  perGithubProxy   = f GithubProxyService
        ,  perGithubSync    = f GithubSyncService
        ,  perHours         = f HoursService
        ,  perHoursApi      = f HoursApiService
        ,  perHC            = f HCService
        ,  perPersonio      = f PersonioService
        ,  perPersonioProxy = f PersonioProxyService
        ,  perPlanmill      = f PlanmillService
        ,  perPlanmillProxy = f PlanmillProxyService
        ,  perPlanmillSync  = f PlanmillSyncService
        ,  perPower         = f PowerService
        ,  perProxMgmt      = f ProxMgmtService
        ,  perProx          = f ProxService
        ,  perReports       = f ReportsService
        ,  perTheme         = f ThemeService
        }

instance FromJSON a => FromJSON (PerService a) where
    parseJSON = withObjectDump "PerService" $ \obj ->
        for (tabulate id) $ \k -> obj .: serviceToText k
