{-# LANGUAGE DataKinds #-}
module Futurice.App.Badge.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Client        (BaseUrl)

-- import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgIntegrationsCfg :: !(IntegrationsConfig '[Proxy, Proxy, I, Proxy, Proxy, I])
    , cfgMockUser        :: !(Maybe FUM.Login)
    , cfgAvatarBaseurl   :: !BaseUrl
    -- , cfgAccessGroups    :: ![FUM.GroupName]
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "AVATAR_BASEURL"
        -- <*> envVar "ACCESS_GROUPS"

