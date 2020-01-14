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
    MonadGitHub(..),
    MonadGoogle(..),
    MonadOkta(..),
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
import PlanMill.Queries.Haxl  (PlanmillBatchError (..))
import Power                  (MonadPower (..))
import Prelude ()

import qualified GitHub as GH

class (Monad m, All (MonadGitHubC m) GHTypes) => MonadGitHub m where
    type MonadGitHubC m :: * -> Constraint
    githubReq :: (MonadGitHubC m a, FromJSON a) => GH.Request 'GH.RA a -> m a
