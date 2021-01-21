{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Integrations.Monad.StateSet (
    stateSetFlowdock,
    stateSetFUM,
    stateSetFUM6,
    stateSetGitHub,
    stateSetGoogle,
    stateSetOkta,
    stateSetPeakon,
    stateSetPersonio,
    stateSetPlanMill,
    stateSetPower,
    stateSetSlack,
    ) where

import Futurice.Prelude
import Network.HTTP.Client   (Request)
import PlanMill.Queries.Haxl (initDataSourceBatch)
import Prelude ()

import qualified Chat.Flowdock.REST           as FD
import qualified FUM
import qualified FUM.Haxl
import qualified Flowdock.Haxl                as FD.Haxl
import qualified Futurice.FUM.MachineAPI      as FUM6
import qualified Futurice.Integrations.GitHub as GH
import qualified Google.Haxl
import qualified Google.Types
import qualified Haxl.Core                    as H
import qualified Okta.Haxl
import qualified Okta.Types
import qualified Peakon.Haxl
import qualified Peakon.Types
import qualified Personio.Haxl
import qualified Power.Haxl
import qualified Slack.Haxl
import qualified Slack.Types

import Futurice.Integrations.Serv

stateSetFlowdock
    :: Logger -> Manager
    -> FD.AuthToken
    -> Tagged ss H.StateStore
    -> Tagged (ServFD ': ss) H.StateStore
stateSetFlowdock _lgr mgr token (Tagged store) = Tagged $
    H.stateSet (FD.Haxl.initDataSource' mgr token) store

stateSetFUM
    :: Logger -> Manager
    -> FUM.AuthToken
    -> FUM.BaseUrl
    -> Tagged ss H.StateStore
    -> Tagged (ServFUM ': ss) H.StateStore
stateSetFUM _lgr mgr token burl (Tagged store) = Tagged $
    H.stateSet (FUM.Haxl.initDataSource' mgr token burl) store

stateSetFUM6
    :: Logger -> Manager
    -> Request
    -> Tagged ss H.StateStore
    -> Tagged (ServFUM6 ': ss) H.StateStore
stateSetFUM6 lgr mgr req (Tagged store) = Tagged $
    H.stateSet (FUM6.initDataSource lgr mgr req) store

stateSetGitHub
    :: Logger -> Manager
    -> Request
    -> Tagged ss H.StateStore
    -> Tagged (ServGH ': ss) H.StateStore
stateSetGitHub lgr mgr req (Tagged store) = Tagged $
    H.stateSet (GH.initDataSource lgr mgr req) store

stateSetPersonio
    :: Logger -> Manager
    -> Request
    -> Tagged ss H.StateStore
    -> Tagged (ServPE ': ss) H.StateStore
stateSetPersonio lgr mgr req (Tagged store) = Tagged $
    H.stateSet (Personio.Haxl.initDataSource lgr mgr req) store

stateSetPlanMill
    :: Logger -> Manager
    -> Request
    -> Tagged ss H.StateStore
    -> Tagged (ServPM ': ss) H.StateStore
stateSetPlanMill lgr mgr req (Tagged store) = Tagged $
    H.stateSet (initDataSourceBatch lgr mgr req) store

stateSetPower
    :: Logger -> Manager
    -> Request
    -> Tagged ss H.StateStore
    -> Tagged (ServPO ': ss) H.StateStore
stateSetPower lgr mgr burl (Tagged store) = Tagged $
    H.stateSet (Power.Haxl.initDataSource lgr mgr burl) store

stateSetGoogle
    :: Logger -> Manager
    -> Google.Types.GoogleCredentials
    -> Tagged ss H.StateStore
    -> Tagged (ServGO ': ss) H.StateStore
stateSetGoogle _lgr mgr cred (Tagged store) = Tagged $
    H.stateSet (Google.Haxl.initDataSource cred mgr) store

stateSetOkta
    :: Logger -> Manager
    -> Okta.Types.OktaCfg
    -> Tagged ss H.StateStore
    -> Tagged (ServOK ': ss) H.StateStore
stateSetOkta lgr mgr cfg (Tagged store) = Tagged $
    H.stateSet (Okta.Haxl.initDataSource cfg lgr mgr) store

stateSetPeakon
    :: Logger -> Manager
    -> Peakon.Types.PeakonCfg
    -> Tagged ss H.StateStore
    -> Tagged (ServPK ': ss) H.StateStore
stateSetPeakon lgr mgr cfg (Tagged store) = Tagged $
    H.stateSet (Peakon.Haxl.initDataSource cfg lgr mgr) store

stateSetSlack
    :: Logger -> Manager
    -> Slack.Types.SlackToken
    -> Tagged ss H.StateStore
    -> Tagged (ServSL ': ss) H.StateStore
stateSetSlack lgr mgr token (Tagged store) = Tagged $
    H.stateSet (Slack.Haxl.initDataSource token lgr mgr) store
