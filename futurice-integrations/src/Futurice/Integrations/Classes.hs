{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | Different monad classes
--
-- /TODO:/ add error handling to classes
module Futurice.Integrations.Classes (
    MonadFUM(..),
    MonadFlowdock(..),
    MonadGitHub(..),
    MonadGoogle(..),
    MonadOkta(..),
    MonadPeakon(..),
    MonadPersonio(..),
    MonadPower(..),
    MonadPlanMillQuery(..),
    MonadTime(..),
    MonadMemoize(..),
    -- * Exceptions
    PlanmillBatchError (..),
    ) where

import Control.Monad.FUM      (MonadFUM (..))
import Control.Monad.Memoize  (MonadMemoize (..))
import Control.Monad.Personio (MonadPersonio (..))
import Control.Monad.PlanMill (MonadPlanMillQuery (..))
import Data.Constraint        (Constraint)
import Futurice.Generics
import Futurice.GitHub        (GHTypes)
import Futurice.Prelude
import Generics.SOP           (All)
import Google                 (MonadGoogle (..))
import Okta                   (MonadOkta (..))
import Peakon                 (MonadPeakon (..))
import PlanMill.Queries.Haxl  (PlanmillBatchError (..))
import Power                  (MonadPower (..))
import Prelude ()

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

class (Monad m, All (MonadGitHubC m) GHTypes) => MonadGitHub m where
    type MonadGitHubC m :: * -> Constraint
    githubReq :: (MonadGitHubC m a, FromJSON a) => GH.Request 'GH.RA a -> m a

class Monad m => MonadFlowdock m where
    flowdockOrganisationReq
        :: FD.ParamName FD.Organisation
        -> m FD.Organisation

    flowdockMessagesSinceReq
        :: FD.ParamName FD.Organisation
        -> FD.ParamName FD.Flow
        -> Maybe FD.MessageId
        -> m [FD.Message]
