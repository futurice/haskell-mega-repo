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

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified Google.Types

import Futurice.Integrations.Serv

data IntegrationsConfig :: [Serv] -> Type where
    IntCfgEmpty
        :: IntegrationsConfig '[]

    IntCfgFlowdock
        :: FD.AuthToken
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFD ': ss)

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

    IntCfgPersonio
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPE ': ss)

    IntCfgPlanMill
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPM ': ss)

    IntCfgPower
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPO ': ss)

-- ServSet is to fail early on error
instance (All ServI ss, ServSet ss) => Configure (IntegrationsConfig ss) where
    configure = go
      where
        go :: forall ss'. All ServI ss' => ConfigParser (IntegrationsConfig ss')
        go = case sList :: SList ss' of
            SNil  -> pure IntCfgEmpty
            SCons -> cons sserv <*> go

        cons :: SServ s -> ConfigParser (IntegrationsConfig ss' -> IntegrationsConfig (s : ss'))
        cons SServFD = IntCfgFlowdock
            <$> envVar "FD_AUTH_TOKEN"
        cons SServFUM = IntCfgFUM
            <$> envVar "FUM_TOKEN"
            <*> envVar "FUM_BASEURL"
        cons SServFUM6 = IntCfgFUM6
            <$> (f <$> envVar "FUMCARBON_HAXLURL")
        cons SServGH = IntCfgGitHub
            <$> (f <$> envVar "GITHUBPROXY_HAXLURL")
        cons SServGO = IntCfgGoogle
            <$> configure
        cons SServPE = IntCfgPersonio
            <$> (f <$> envVar "PERSONIOPROXY_REQUESTURL")
        cons SServPM = IntCfgPlanMill
            <$> (f <$> envVar "PLANMILLPROXY_HAXLURL")
        cons SServPO = IntCfgPower
            <$> (f <$> envVar "POWER_API_BASEURL")

        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }

-- | A helper useful in REPL.
loadIntegrationConfig
    :: Logger
    -> IO (IntegrationsConfig AllServs)
loadIntegrationConfig lgr =
    runLogT "loadIntegrationConfig" lgr $ getConfig "REPL"
