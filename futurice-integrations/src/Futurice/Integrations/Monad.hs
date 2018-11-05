{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Integrations.Monad (
    Integrations,
    Env,
    runIntegrations,
    runIntegrationsWithHaxlStore,
    integrationConfigToState,
    runIntegrationsIO,
    IntegrationsConfig (..),
    loadIntegrationConfig,
    -- * state setters
    stateSetFlowdock,
    stateSetFUM,
    stateSetFUM6,
    stateSetGitHub,
    stateSetPersonio,
    stateSetPlanMill,
    ) where

import Control.Monad.PlanMill    (MonadPlanMillConstraint (..))
import Data.Constraint
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.Has              (FlipIn)
import Futurice.Prelude
import Futurice.TypeTag
import Prelude ()

import qualified Chat.Flowdock.REST           as FD
import qualified Flowdock.Haxl                as FD.Haxl
import qualified FUM
import qualified FUM.Haxl
import qualified Futurice.FUM.MachineAPI      as FUM6
import qualified Futurice.GitHub              as GH
import qualified Futurice.Integrations.GitHub as GH
import qualified Haxl.Core                    as H
import qualified Personio
import qualified Power.Haxl
import qualified Personio.Haxl
import qualified PlanMill.Types.Query         as Q

import Futurice.Integrations.Classes
import Futurice.Integrations.Common
import Futurice.Integrations.Serv
import Futurice.Integrations.Monad.StateSet
import Futurice.Integrations.Serv.Config
import Futurice.Integrations.Serv.Contains

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

-- | Opaque environment, exported for haddock
--
data Env = Env
    { _envNow                 :: !UTCTime
    , _envFumEmployeeListName :: !FUM.ListName
    , _envFlowdockOrgName     :: !(FD.ParamName FD.Organisation)
    , _envGithubOrgName       :: !(GH.Name GH.Organization)
    }

makeLenses ''Env

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- | Integrations monad
--
-- We fake type level set.
--
newtype Integrations (ss :: [Serv]) a
    = Integr { unIntegr :: ReaderT Env (H.GenHaxl ()) a }

-- | Lift arbitrary haxl computations into 'Integrations'. This is potentially unsafe.
liftHaxl :: H.GenHaxl () a -> Integrations ss a
liftHaxl = Integr . lift

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

runIntegrations
    :: forall ss a. ServSet ss
    => Manager -> Logger -> UTCTime
    -> IntegrationsConfig ss
    -> Integrations ss a
    -> IO a
runIntegrations mgr lgr now cfg m =
    runIntegrationsWithHaxlStore now (integrationConfigToState mgr lgr cfg) m

integrationConfigToState
    :: ServSet ss
    => Manager -> Logger
    -> IntegrationsConfig ss -> Tagged ss H.StateStore
integrationConfigToState mgr lgr cfg0 = flip runCTS cfg0 $
    withServSet ctsEmpty $ \_ (CTS f) -> CTS $ \cfg1 -> case cfg1 of
        IntCfgFlowdock token cfg2 -> stateSetFlowdock lgr mgr token (f cfg2)
        IntCfgFUM token burl cfg2 -> stateSetFUM lgr mgr token burl (f cfg2)
        IntCfgFUM6 req cfg2       -> stateSetFUM6 lgr mgr req (f cfg2)
        IntCfgGitHub req cfg2     -> stateSetGitHub lgr mgr req (f cfg2)
        IntCfgPersonio req cfg2   -> stateSetPersonio lgr mgr req (f cfg2)
        IntCfgPlanMill req cfg2   -> stateSetPlanMill lgr mgr req (f cfg2)
        IntCfgPower req cfg2      -> stateSetPower lgr mgr req (f cfg2)
  where
    ctsEmpty :: ConfigToState '[]
    ctsEmpty = CTS $ \IntCfgEmpty -> Tagged H.stateEmpty

newtype ConfigToState ss = CTS { runCTS :: IntegrationsConfig ss -> Tagged ss H.StateStore }

-- | Run 'Integrations' action using Haxl's 'H.StateStore'.
-- This function is needed when one want to do un-orthodox integrations.
runIntegrationsWithHaxlStore
    :: UTCTime
    -> Tagged ss H.StateStore
    -> Integrations ss a
    -> IO a
runIntegrationsWithHaxlStore now (Tagged stateStore) (Integr m) = do
    let env = Env
            { _envNow = now
            -- HACK: these are hardcoded
            , _envFumEmployeeListName = "employees"
            , _envFlowdockOrgName     = FD.mkParamName "futurice"
            , _envGithubOrgName       = "futurice"
            }
    let haxl = runReaderT m env
    haxlEnv <- H.initEnv stateStore ()
    H.runHaxl haxlEnv haxl

{-# DEPRECATED runIntegrationsIO "Only use this in repl" #-}
runIntegrationsIO :: Integrations AllServs a -> IO a
runIntegrationsIO action = withStderrLogger $ \lgr -> do
    cfg <- loadIntegrationConfig lgr
    mgr <- newManager tlsManagerSettings
    now <- currentTime
    runIntegrations mgr lgr now cfg action

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor (Integrations idxs) where
    fmap f (Integr x) = Integr (fmap f x)

instance Applicative (Integrations idxs)  where
    pure = Integr . pure
    Integr f <*> Integr x = Integr (f <*> x)
    Integr f  *> Integr x = Integr (f  *> x)

instance Monad (Integrations idxs) where
    return = pure
    (>>) = (*>)
    Integr f >>= k = Integr $ f >>= unIntegr . k

-------------------------------------------------------------------------------
-- MonadTime
-------------------------------------------------------------------------------

instance MonadTime (Integrations ss) where
    currentTime = view envNow

-------------------------------------------------------------------------------
-- MonadPlanMillQuery
-------------------------------------------------------------------------------

instance Contains ServPM ss => MonadPlanMillConstraint (Integrations ss) where
    type MonadPlanMillC (Integrations ss) = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance Contains ServPM ss => MonadPlanMillQuery (Integrations ss) where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> liftHaxl (H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

-------------------------------------------------------------------------------
-- MonadFUM
-------------------------------------------------------------------------------

instance Contains ServFUM ss  => MonadFUM (Integrations ss) where
    fumAction = liftHaxl . FUM.Haxl.request

-------------------------------------------------------------------------------
-- MonadFlowdock
-------------------------------------------------------------------------------

instance Contains ServFD ss => MonadFlowdock (Integrations ss) where
    flowdockOrganisationReq = liftHaxl . FD.Haxl.organisation
    flowdockMessagesSinceReq org flow since = liftHaxl $
        FD.Haxl.messagesSince org flow since

-------------------------------------------------------------------------------
-- MonadFUM6
-------------------------------------------------------------------------------

instance Contains ServFUM6 ss => FUM6.MonadFUM6 (Integrations ss) where
    fum6 = liftHaxl . FUM6.fumHaxlRequest

-------------------------------------------------------------------------------
-- MonadGitHub
-------------------------------------------------------------------------------

instance Contains ServGH ss => MonadGitHub (Integrations ss) where
    type MonadGitHubC (Integrations ss) = FlipIn GH.GHTypes
    githubReq req = case (showDict, typeableDict) of
        (Dict, Dict) -> liftHaxl (H.dataFetch $ GH.GHR tag req)
      where
        tag = GH.mkReqTag
        showDict     = typeTagDict (Proxy :: Proxy Show) tag
        typeableDict = typeTagDict (Proxy :: Proxy Typeable) tag

-------------------------------------------------------------------------------
-- MonadPersonio
-------------------------------------------------------------------------------

instance Contains ServPE ss => MonadPersonio (Integrations ss) where
    personio r = case (showDict, typeableDict) of
        (Dict, Dict) -> liftHaxl . Personio.Haxl.request $ r
      where
        showDict     = Personio.requestDict (Proxy :: Proxy Show) r
        typeableDict = Personio.requestDict (Proxy :: Proxy Typeable) r

-------------------------------------------------------------------------------
-- MonadPower
-------------------------------------------------------------------------------

instance Contains ServPO ss => MonadPower (Integrations ss) where
    powerReq = liftHaxl . Power.Haxl.request

-------------------------------------------------------------------------------
-- Has* instances
-------------------------------------------------------------------------------

instance MonadReader Env (Integrations ss) where
    ask = Integr ask
    local f = Integr . local f . unIntegr

instance HasFUMEmployeeListName Env where
    fumEmployeeListName = envFumEmployeeListName

instance HasFlowdockOrgName Env where
    flowdockOrganisationName = envFlowdockOrgName

instance HasGithubOrgName Env where
    githubOrganisationName = envGithubOrgName
