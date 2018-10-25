{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Futurice.App.Proxy.Ctx where

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Prelude
import Futurice.Services          (Service (..))
import Prelude ()

import Futurice.App.Proxy.Endpoint (HasClientBaseurl (..), HasHttpManager (..))

import Futurice.App.Proxy.Config
-- | Context type, holds http manager and baseurl configurations
data Ctx = Ctx
    { _ctxManager              :: !Manager
    , ctxPostgresPool         :: !(Pool Connection)
    , ctxLogger               :: !Logger
    -- Base URLS
    , _ctxConfig              :: !Config
    }

makeLenses ''Ctx

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance HasHttpManager Ctx where
    httpManager = ctxManager

instance HasClientBaseurl Ctx 'AvatarService        where clientBaseurl _ = ctxConfig . cfgAvatarBaseurl
instance HasClientBaseurl Ctx 'ReportsService       where clientBaseurl _ = ctxConfig . cfgReportsAppBaseurl
instance HasClientBaseurl Ctx 'FumCarbonService     where clientBaseurl _ = ctxConfig . cfgFumCarbonBaseurl
instance HasClientBaseurl Ctx 'PlanmillProxyService where clientBaseurl _ = ctxConfig . cfgPlanmillProxyBaseurl
instance HasClientBaseurl Ctx 'GithubProxyService   where clientBaseurl _ = ctxConfig . cfgGithubProxyBaseurl
instance HasClientBaseurl Ctx 'PowerService         where clientBaseurl _ = ctxConfig . cfgPowerBaseurl
instance HasClientBaseurl Ctx 'PersonioProxyService where clientBaseurl _ = ctxConfig . cfgPersonioProxyBaseurl
instance HasClientBaseurl Ctx 'ContactsApiService   where clientBaseurl _ = ctxConfig . cfgContactsApiBaseurl
instance HasClientBaseurl Ctx 'SmsProxyService      where clientBaseurl _ = ctxConfig . cfgSmsProxyBaseurl
