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
    stateSetPersonio,
    stateSetPlanMill,
    stateSetPower,
    ) where

import Futurice.Prelude
import Network.HTTP.Client   (Request)
import PlanMill.Queries.Haxl (initDataSourceBatch)
import Prelude ()

import qualified Chat.Flowdock.REST           as FD
import qualified Flowdock.Haxl                as FD.Haxl
import qualified FUM
import qualified FUM.Haxl
import qualified Futurice.FUM.MachineAPI      as FUM6
import qualified Futurice.Integrations.GitHub as GH
import qualified Haxl.Core                    as H
import qualified Personio.Haxl
import qualified Power.Haxl

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
