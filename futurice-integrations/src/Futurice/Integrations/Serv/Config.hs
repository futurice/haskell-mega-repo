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
import qualified Futurice.GitHub    as GH

import Futurice.Integrations.Serv

data IntegrationsConfig :: [Serv] -> Type where
    IntCfgEmpty
        :: IntegrationsConfig '[]

    IntCfgFlowdock
        :: FD.AuthToken
        -> FD.ParamName FD.Organisation
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFD ': ss)

    IntCfgFUM
        :: FUM.AuthToken
        -> FUM.BaseUrl
        -> FUM.ListName
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFUM ': ss)

    IntCfgFUM6
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServFUM6 ': ss)

    IntCfgGitHub
        :: Request
        -> GH.Name GH.Organization
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServGH ': ss)

    IntCfgPersonio
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPE ': ss)

    IntCfgPlanMill
        :: Request
        -> IntegrationsConfig ss
        -> IntegrationsConfig (ServPM ': ss)

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
            <*> envVar "FD_ORGANISATION"
        cons SServFUM = IntCfgFUM
            <$> envVar "FUM_TOKEN"
            <*> envVar "FUM_BASEURL"
            <*> envVar "FUM_LISTNAME"
        cons SServFUM6 = IntCfgFUM6
            <$> (f <$> envVar "FUMCARBON_HAXLURL")
        cons SServGH = IntCfgGitHub
            <$> (f <$> envVar "GITHUBPROXY_HAXLURL")
            <*> envVar "GH_ORG"
        cons SServPE = IntCfgPersonio
            <$> (f <$> envVar "PERSONIOPROXY_REQUESTURL")
        cons SServPM = IntCfgPlanMill
            <$> (f <$> envVar "PLANMILLPROXY_HAXLURL")

        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }

-- | A helper useful in REPL.
loadIntegrationConfig
    :: Logger
    -> IO (IntegrationsConfig '[ ServFD, ServFUM, ServFUM6, ServGH, ServPE, ServPM ])
loadIntegrationConfig lgr =
    runLogT "loadIntegrationConfig" lgr $ getConfig "REPL"
