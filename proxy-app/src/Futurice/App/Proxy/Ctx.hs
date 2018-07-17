{-# LANGUAGE TemplateHaskell, DataKinds, MultiParamTypeClasses #-}
module Futurice.App.Proxy.Ctx where

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Prelude
import Futurice.Services          (Service (..))
import Prelude ()
import Servant.Client             (BaseUrl)

import Futurice.App.Proxy.Endpoint (HasClientBaseurl (..), HasHttpManager (..))

-- | Context type, holds http manager and baseurl configurations
data Ctx = Ctx
    { ctxManager              :: !Manager
    , ctxPostgresPool         :: !(Pool Connection)
    , ctxLogger               :: !Logger
    -- Base URLS
    , _ctxReportsAppBaseurl    :: !BaseUrl
    , _ctxFumCarbonBaseurl     :: !BaseUrl
    , _ctxPlanmillProxyBaseurl :: !BaseUrl
    , _ctxGithubProxyBaseurl   :: !BaseUrl
    , _ctxPersonioProxyBaseurl :: !BaseUrl
    , _ctxPowerBaseurl         :: !BaseUrl
    , _ctxContactsApiBaseurl   :: !BaseUrl
    }

makeLenses ''Ctx

instance HasHttpManager Ctx where
    httpManager = lens ctxManager $ \ctx x -> ctx { ctxManager = x }

instance HasClientBaseurl Ctx 'ReportsService where
    clientBaseurl _ = ctxReportsAppBaseurl

instance HasClientBaseurl Ctx 'FumCarbonService where
    clientBaseurl _ = ctxFumCarbonBaseurl

instance HasClientBaseurl Ctx 'PlanmillProxyService where
    clientBaseurl _ = ctxPlanmillProxyBaseurl

instance HasClientBaseurl Ctx 'GithubProxyService where
    clientBaseurl _ = ctxGithubProxyBaseurl

instance HasClientBaseurl Ctx 'PowerService where
    clientBaseurl _ = ctxPowerBaseurl

instance HasClientBaseurl Ctx 'PersonioProxyService where
    clientBaseurl _ = ctxPersonioProxyBaseurl

instance HasClientBaseurl Ctx 'ContactsApiService where
    clientBaseurl _ = ctxContactsApiBaseurl
