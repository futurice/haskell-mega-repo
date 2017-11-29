module Futurice.App.Proxy.Ctx where

import Prelude ()
import Futurice.Prelude
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant.Client             (BaseUrl)
import Servant.Proxy              (HasHttpManager (..))

import qualified FUM

-- | Context type, holds http manager and baseurl configurations
data Ctx = Ctx
    { ctxManager              :: !Manager
    , ctxPostgresPool         :: !(Pool Connection)
    , ctxReportsAppBaseurl    :: !BaseUrl
    , ctxFumCarbonBaseurl     :: !BaseUrl
    , ctxPlanmillProxyBaseurl :: !BaseUrl
    , ctxGithubProxyBaseurl   :: !BaseUrl
    , ctxPersonioProxyBaseurl :: !BaseUrl
    , ctxFumBaseurl           :: !BaseUrl
    , ctxFumAuthToken         :: !FUM.AuthToken
    , ctxPowerBaseurl         :: !BaseUrl
    , ctxContactsApiBaseurl   :: !BaseUrl
    , ctxLogger               :: !Logger
    }

instance HasHttpManager Ctx where
    httpManager = lens ctxManager $ \ctx x -> ctx { ctxManager = x }
