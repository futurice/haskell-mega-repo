{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Futurice.App.Reports.Config (
    Config(..),
    ReportIntegrations,
    toFutuquCfg,
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futuqu                     (FutuquIntegrations)
import Futurice.Email             (Email)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Client             (BaseUrl)

import qualified Futurice.Integrations.Serv.Config as C

type ReportIntegrations = '[ ServFD, ServFUM, ServGH, ServPE, ServPK, ServPM, ServPO ]

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig ReportIntegrations)
    , cfgEmailProxyBaseurl     :: !BaseUrl
    , cfgSmsProxyBaseurl       :: !BaseUrl
    , cfgPreferencesAppBaseurl :: !BaseUrl
    , cfgPostgresConnInfo      :: !ConnectInfo
    , cfgPostgresConnInfoInv   :: !ConnectInfo
    , cfgHcEmailCC             :: !(Maybe Email)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "EMAILPROXY_BASEURL"
        <*> envVar "SMSPROXY_BASEURL"
        <*> envVar "PREFERENCES_BASEURL"
        <*> envConnectInfo
        <*> envConnectInfo' "INVENTORY_"
        <*> optionalAlt (envVar "HCEMAIL_CC")

toFutuquCfg
    :: C.IntegrationsConfig ReportIntegrations
    -> C.IntegrationsConfig FutuquIntegrations
toFutuquCfg
    (C.IntCfgFlowdock _
    (C.IntCfgFUM _ _
    (C.IntCfgGitHub _
    (C.IntCfgPersonio a
    (C.IntCfgPeakon _ c))))) = (C.IntCfgPersonio a c) --c
