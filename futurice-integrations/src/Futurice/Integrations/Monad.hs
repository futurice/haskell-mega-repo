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
    runIntegrationsIO,
    IntegrationsConfig (..),
    loadIntegrationConfig,
    -- * state morphisms
    IntegrationStateMorphism (..),
    pmIntegrationStateMorphism,
    fumIntegrationStateMorphism,
    fum6IntegrationStateMorphism,
    fdIntegrationStateMorphism,
    ghIntegrationStateMorphism,
    peIntegrationStateMorphism,
    ) where

import Control.Monad.PlanMill    (MonadPlanMillConstraint (..))
import Data.Constraint
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.EnvConfig
import Futurice.Has              (FlipIn)
import Futurice.Prelude
import Futurice.TypeTag
import Generics.SOP.Lens         (uni)
import Network.HTTP.Client
       (Request, responseTimeout, responseTimeoutMicro)
import PlanMill.Queries.Haxl     (initDataSourceBatch)
import Prelude ()

import qualified Chat.Flowdock.REST           as FD
import qualified Control.Category             as C
import qualified Data.Type.Nat                as N
import qualified Flowdock.Haxl                as FD.Haxl
import qualified FUM
import qualified FUM.Haxl
import qualified Futurice.FUM.MachineAPI      as FUM6
import qualified Futurice.GitHub              as GH
import qualified Futurice.Integrations.GitHub as GH
import qualified Haxl.Core                    as H
import qualified Personio
import qualified Personio.Haxl
import qualified PlanMill.Types.Query         as Q

import Futurice.Integrations.Classes
import Futurice.Integrations.Common

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

-- | Opaque environment, exported for haddock
--
-- Currently no PlanMill environment.
data Env fum gh fd = Env
    { _envFumEmployeeListName :: !(fum :$ FUM.ListName)
    , _envFlowdockOrgName     :: !(fd :$ FD.ParamName FD.Organisation)
    , _envGithubOrgName       :: !(gh :$ GH.Name GH.Organization)
    , _envNow                 :: !UTCTime
    }

makeLenses ''Env

-------------------------------------------------------------------------------
-- Type Families
-------------------------------------------------------------------------------

type IdxPM   = N.Nat0
type IdxFUM  = N.Nat1
type IdxFUM6 = N.Nat2
type IdxGH   = N.Nat3
type IdxFD   = N.Nat4
type IdxPE   = N.Nat5

type family Nth (n :: N.Nat) (fs :: [* -> *]) :: * -> * where
    Nth n        '[]       = Proxy
    Nth 'N.Z     (f ': fs) = f
    Nth ('N.S n) (f ': fs) = Nth n fs

type NthPM   fs = Nth IdxPM   fs
type NthFUM  fs = Nth IdxFUM  fs
type NthFUM6 fs = Nth IdxFUM6 fs
type NthGH   fs = Nth IdxGH   fs
type NthFD   fs = Nth IdxFD   fs
type NthPE   fs = Nth IdxPE   fs

type family NthSet (n :: N.Nat) (g :: * -> *) (fs :: [* -> *]) :: [* -> *] where
    NthSet 'N.Z     g '[]       = '[g]
    NthSet ('N.S n) g '[]       = Proxy ': NthSet n g '[]
    NthSet 'N.Z     g (f ': fs) = g ': fs
    NthSet ('N.S n) g (f ': fs) = f ': NthSet n g fs

type NthSetPM   g fs = NthSet IdxPM   g fs
type NthSetFUM  g fs = NthSet IdxFUM  g fs
type NthSetFUM6 g fs = NthSet IdxFUM6 g fs
type NthSetGH   g fs = NthSet IdxGH   g fs
type NthSetFD   g fs = NthSet IdxFD   g fs
type NthSetPE   g fs = NthSet IdxPE   g fs

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- | Integrations monad
--
-- type parameters indicate whether that integration is enabled:
-- 'I' yes, 'Proxy' no
--
-- We'd like to have row types in Haskell, but for now we use positional arguments.
newtype Integrations idxs a
    = Integr { unIntegr :: ReaderT (Env (NthFUM idxs) (NthGH idxs) (NthFD idxs)) (H.GenHaxl ()) a }

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- | TODO: Show instance
data IntegrationsConfig idxs = MkIntegrationsConfig
    -- Planmill
    { integrCfgPlanmillProxyBaseRequest :: !(NthPM idxs Request)
    -- FUM
    , integrCfgFumAuthToken             :: !(NthFUM idxs FUM.AuthToken)
    , integrCfgFumBaseUrl               :: !(NthFUM idxs FUM.BaseUrl)
    , integrCfgFumEmployeeListName      :: !(NthFUM idxs FUM.ListName)
    -- FUM Carbon
    , integrCfgFumCarbonBaseRequest     :: !(NthFUM6 idxs Request)
    -- GitHub
    , integrCfgGithubProxyBaseRequest   :: !(NthGH idxs Request)
    , integrCfgGithubOrgName            :: !(NthGH idxs :$ GH.Name GH.Organization)
    -- Flowdock
    , integrCfgFlowdockToken            :: !(NthFD idxs FD.AuthToken)
    , integrCfgFlowdockOrgName          :: !(NthFD idxs :$ FD.ParamName FD.Organisation)
    -- Personio
    , integrCfgPersonioProxyBaseRequest :: !(NthPE idxs Request)
    }

runIntegrations
    :: (SFunctorI pm, SFunctorI fum, SFunctorI fum6, SFunctorI gh, SFunctorI fd, SFunctorI pe)
    => Manager -> Logger -> UTCTime
    -> IntegrationsConfig '[pm, fum, fum6, gh, fd, pe]
    -> Integrations '[pm, fum, fum6, gh, fd, pe] a
    -> IO a
runIntegrations mgr lgr now cfg m =
    runIntegrationsWithHaxlStore now stateMorphism cfg m
  where
    stateMorphism
        = pmIntegrationStateMorphism     lgr mgr cfg
        C.. fumIntegrationStateMorphism  lgr mgr cfg
        C.. fum6IntegrationStateMorphism lgr mgr cfg
        C.. fdIntegrationStateMorphism   lgr mgr cfg
        C.. ghIntegrationStateMorphism   lgr mgr cfg
        C.. peIntegrationStateMorphism   lgr mgr cfg

-- | Run 'Integrations' action using Haxl's 'H.StateStore'.
-- This function is needed when one want to do un-orthodox integrations.
runIntegrationsWithHaxlStore
    :: UTCTime
    -> IntegrationStateMorphism '[] idxs
    -> IntegrationsConfig idxs
    -> Integrations idxs a
    -> IO a
runIntegrationsWithHaxlStore now (IntegrSM f) cfg (Integr m) = do
    let env = Env
            { _envFumEmployeeListName = integrCfgFumEmployeeListName cfg
            , _envNow                 = now
            , _envFlowdockOrgName     = integrCfgFlowdockOrgName cfg
            , _envGithubOrgName       = integrCfgGithubOrgName cfg
            }
    let haxl = runReaderT m env
    haxlEnv <- H.initEnv (f H.stateEmpty) ()
    H.runHaxl haxlEnv haxl

{-# DEPRECATED runIntegrationsIO "Only use this in repl" #-}
runIntegrationsIO :: Integrations '[I, I, I, I, I, I] a -> IO a
runIntegrationsIO action = withStderrLogger $ \lgr -> do
    cfg <- loadIntegrationConfig lgr
    mgr <- newManager tlsManagerSettings
    now <- currentTime
    runIntegrations mgr lgr now cfg action

-------------------------------------------------------------------------------
-- env-config
-------------------------------------------------------------------------------
--
-- | A helper useful in REPL.
loadIntegrationConfig :: Logger -> IO (IntegrationsConfig '[I, I, I, I, I, I])
loadIntegrationConfig lgr =
    runLogT "loadIntegrationConfig" lgr $ getConfig "REPL"

instance
    (SFunctorI pm, SFunctorI fum, SFunctorI fum6, SFunctorI gh, SFunctorI fd, SFunctorI pe)
    => Configure (IntegrationsConfig '[pm, fum, fum6, gh, fd, pe])
  where
    configure = MkIntegrationsConfig
        <$> (f <$$> envVar' "PLANMILLPROXY_HAXLURL")
        <*> envVar' "FUM_TOKEN"
        <*> envVar' "FUM_BASEURL"
        <*> envVar' "FUM_LISTNAME"
        <*> (f <$$> envVar' "FUMCARBON_HAXLURL")
        <*> (f <$$> envVar' "GITHUBPROXY_HAXLURL")
        <*> envVar' "GH_ORG"
        <*> envVar' "FD_AUTH_TOKEN"
        <*> envVar' "FD_ORGANISATION"
        <*> (f <$$> envVar' "PERSONIOPROXY_REQUESTURL")
      where
        f req = req { responseTimeout = responseTimeoutMicro $ 300 * 1000000 }

        envVar' :: forall f a. (FromEnvVar a, SFunctorI f) => String -> ConfigParser (f a)
        envVar' name = case sfunctor :: SFunctor f of
            SP -> pure Proxy
            SI -> I <$> envVar name

-------------------------------------------------------------------------------S
-- Functor singletons
-------------------------------------------------------------------------------

data SFunctor f where
    SI :: SFunctor I
    SP :: SFunctor Proxy

class Applicative f => SFunctorI f     where sfunctor :: SFunctor f
instance               SFunctorI I     where sfunctor = SI
instance               SFunctorI Proxy where sfunctor = SP

extractSEndo :: SFunctorI f => f (a -> a) -> a -> a
extractSEndo = extractSFunctor id

extractSFunctor :: forall f a. SFunctorI f => a -> f a -> a
extractSFunctor def f = case sfunctor :: SFunctor f of
    SP -> def
    SI -> f ^. uni

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

instance MonadTime (Integrations idxs) where
    currentTime = view envNow

-------------------------------------------------------------------------------
-- MonadPlanMillQuery
-------------------------------------------------------------------------------

instance NthPM idxs ~ I => MonadPlanMillConstraint (Integrations idxs) where
    type MonadPlanMillC (Integrations idxs) = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance NthPM idxs ~ I => MonadPlanMillQuery (Integrations idxs) where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr (lift $ H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

-------------------------------------------------------------------------------
-- MonadFUM
-------------------------------------------------------------------------------

instance NthFUM idxs ~ I  => MonadFUM (Integrations idxs) where
    fumAction = Integr . lift . FUM.Haxl.request

-------------------------------------------------------------------------------
-- MonadFlowdock
-------------------------------------------------------------------------------

instance NthFD idxs ~ I => MonadFlowdock (Integrations idxs) where
    flowdockOrganisationReq = Integr . lift . FD.Haxl.organisation

-------------------------------------------------------------------------------
-- MonadFUM6
-------------------------------------------------------------------------------

instance NthFUM6 idxs ~ I => FUM6.MonadFUM6 (Integrations idxs) where
    fum6 = Integr . lift . FUM6.fumHaxlRequest

-------------------------------------------------------------------------------
-- MonadGitHub
-------------------------------------------------------------------------------

instance NthGH idxs ~ I => MonadGitHub (Integrations idxs) where
    type MonadGitHubC (Integrations idxs) = FlipIn GH.GHTypes
    githubReq req = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr (lift $ H.dataFetch $ GH.GHR tag req)
      where
        tag = GH.mkReqTag
        showDict     = typeTagDict (Proxy :: Proxy Show) tag
        typeableDict = typeTagDict (Proxy :: Proxy Typeable) tag

-------------------------------------------------------------------------------
-- MonadPersonio
-------------------------------------------------------------------------------

instance NthPE idxs ~ I => MonadPersonio (Integrations idxs) where
    personio r = case (showDict, typeableDict) of
        (Dict, Dict) -> Integr . lift . Personio.Haxl.request $ r
      where
        showDict     = Personio.requestDict (Proxy :: Proxy Show) r
        typeableDict = Personio.requestDict (Proxy :: Proxy Typeable) r

-------------------------------------------------------------------------------
-- Has* instances
-------------------------------------------------------------------------------

instance (NthFUM idxs ~ fum, NthGH idxs ~ gh, NthFD idxs ~ fd)
    => MonadReader (Env fum gh fd) (Integrations idxs)
  where
    ask = Integr ask
    local f = Integr . local f . unIntegr

instance fum ~ I => HasFUMEmployeeListName (Env fum gh fd) where
    fumEmployeeListName = envFumEmployeeListName . uni

instance fd ~ I => HasFlowdockOrgName (Env fum gh fd) where
    flowdockOrganisationName = envFlowdockOrgName . uni

instance (gh ~ I) => HasGithubOrgName (Env fum gh fd) where
    githubOrganisationName = envGithubOrgName . uni

-------------------------------------------------------------------------------
-- State Morphism
-------------------------------------------------------------------------------

newtype IntegrationStateMorphism (a :: [* -> *]) (b :: [* -> *])
   = IntegrSM (H.StateStore -> H.StateStore)

instance C.Category IntegrationStateMorphism where
    id = IntegrSM id
    IntegrSM f . IntegrSM g = IntegrSM (f . g)

pmIntegrationStateMorphism
    :: (SFunctorI f, NthPM idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetPM f idxs)
pmIntegrationStateMorphism lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ initDataSourceBatch lgr mgr
        <$> integrCfgPlanmillProxyBaseRequest cfg

fumIntegrationStateMorphism
    :: (SFunctorI f, NthFUM idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetFUM f idxs)
fumIntegrationStateMorphism _lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ FUM.Haxl.initDataSource' mgr
        <$> integrCfgFumAuthToken cfg
        <*> integrCfgFumBaseUrl cfg

fum6IntegrationStateMorphism
    :: (SFunctorI f, NthFUM6 idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetFUM6 f idxs)
fum6IntegrationStateMorphism lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ FUM6.initDataSource lgr mgr
        <$> integrCfgFumCarbonBaseRequest cfg

fdIntegrationStateMorphism
    :: (SFunctorI f, NthFD idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetFD f idxs)
fdIntegrationStateMorphism _lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ FD.Haxl.initDataSource' mgr
        <$> integrCfgFlowdockToken cfg

ghIntegrationStateMorphism
    :: (SFunctorI f, NthGH idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetGH f idxs)
ghIntegrationStateMorphism lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ GH.initDataSource lgr mgr
        <$> integrCfgGithubProxyBaseRequest cfg

peIntegrationStateMorphism
    :: (SFunctorI f, NthPE idxs' ~ f)
    => Logger -> Manager
    -> IntegrationsConfig idxs'
    -> IntegrationStateMorphism idxs (NthSetPE f idxs)
peIntegrationStateMorphism lgr mgr cfg =
    IntegrSM $ extractSEndo $ fmap H.stateSet $ Personio.Haxl.initDataSource lgr mgr
        <$> integrCfgPersonioProxyBaseRequest cfg
