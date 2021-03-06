{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Integrations.Serv.Config where

import Data.Kind           (Type)
import Futurice.EnvConfig
import Futurice.Prelude
import Generics.SOP        (All, SList (..), sList)
import Network.HTTP.Client (Request, responseTimeout, responseTimeoutMicro)
import Prelude ()

import qualified FUM
import qualified Google.Types
import qualified Okta.Types
import qualified Peakon.Types
import qualified Slack.Types

import Futurice.Integrations.Serv

data IntegrationsConfig :: [Serv] -> Type where
    IntCfgEmpty
        :: IntegrationsConfig '[]

    IntCfgFUM
        :: FUM.AuthToken
        -> FUM.BaseUrl
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFUM ': ss)

    IntCfgFUM6
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFUM6 ': ss)

    IntCfgGitHub
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServGH ': ss)

    IntCfgGoogle
        :: Google.Types.GoogleCredentials
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServGO ': ss)

    IntCfgOkta
        :: Okta.Types.OktaCfg
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServOK ': ss)

    IntCfgPersonio
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPE ': ss)

    IntCfgPeakon
        :: Peakon.Types.PeakonCfg
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPK ': ss)

    IntCfgPlanMill
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPM ': ss)

    IntCfgPower
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPO ': ss)

    IntCfgSlack
        :: Slack.Types.SlackToken
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServSL ': ss)

-- ServSet is to fail early on error
instance (All ServI ss, ServSet ss) => Configure (IntegrationsConfig ss) where
    configure = go
      where
        go :: forall ss'. All ServI ss' => ConfigParser (IntegrationsConfig ss')
        go = case sList :: SList ss' of
            SNil  -> pure IntCfgEmpty
            SCons -> cons sserv <*> go

        cons :: SServ s -> ConfigParser (IntegrationsConfig ss' -> IntegrationsConfig (s : ss'))
        cons SServFUM = IntCfgFUM
            <$> envVar "FUM_TOKEN"
            <*> envVar "FUM_BASEURL"
        cons SServFUM6 = IntCfgFUM6
            <$> (f <$> envVar "FUMCARBON_HAXLURL")
        cons SServGH = IntCfgGitHub
            <$> (f <$> envVar "GITHUBPROXY_HAXLURL")
        cons SServGO = IntCfgGoogle
            <$> configure
        cons SServOK = IntCfgOkta
            <$> configure
        cons SServPE = IntCfgPersonio
            <$> (f <$> envVar "PERSONIOPROXY_REQUESTURL")
        cons SServPK = IntCfgPeakon
            <$> configure
        cons SServPM = IntCfgPlanMill
            <$> (f <$> envVar "PLANMILLPROXY_HAXLURL")
        cons SServPO = IntCfgPower
            <$> (f <$> envVar "POWER_API_BASEURL")
        cons SServSL = IntCfgSlack
            <$> configure

        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }

-- | A helper useful in REPL.
loadIntegrationConfig
    :: Logger
    -> IO (IntegrationsConfig AllServs)
loadIntegrationConfig lgr =
    runLogT "loadIntegrationConfig" lgr $ getConfig "REPL"
