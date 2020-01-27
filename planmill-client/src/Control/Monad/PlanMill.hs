{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Control.Monad.PlanMill (
    MonadPlanMill(..),
    planmillVectorAction,
    MonadPlanMillQuery(..),
    planmillVectorQuery,
    MonadPlanMillTypes,
    ForallFSymbol(..),
    MonadPlanMillConstraint(..),
    ) where

import Prelude ()
import PlanMill.Internal.Prelude

import Control.Monad.Memoize            (MonadMemoize)
import Data.Constraint                  (Constraint, Dict (..), type (:-)(..), (\\))
import Futurice.Constraint.ForallSymbol (ForallFSymbol (..))
import Futurice.Trans.PureT

import PlanMill.Classes
import PlanMill.Eval
import PlanMill.Test        (evalPlanMillIO)
import PlanMill.Types
import PlanMill.Types.Query (Query)

-- | Types 'MonadPlanMillC' should be satisfied to define 'MonadPlanMill' instance.
--
-- Requiring these reduces boilerplate greatly!
type MonadPlanMillTypes =
    '[ Absence, Assignment, Me, Meta, Project, ProjectMember, ReportableAssignment
     , Task, TimeBalance, Timereport, Team, User, UserCapacity
     ]
-- Note: to update do:
-- intercalate ", " $ sort $ splitOn ", " "User, Team"

-- | Superclass for providing constraints for 'MonadPlanMill' and
-- 'MonadPlanMillQuery'.
class
    ( Monad m
    -- Unfortunately we have to write all of those down
    , MonadPlanMillC m Absence
    , MonadPlanMillC m Account
    , MonadPlanMillC m Assignment
    , MonadPlanMillC m CapacityCalendar
    , MonadPlanMillC m Me
    , MonadPlanMillC m Meta
    , MonadPlanMillC m Project
    , MonadPlanMillC m ProjectMember
    , MonadPlanMillC m ReportableAssignment
    , MonadPlanMillC m Task
    , MonadPlanMillC m TimeBalance
    , MonadPlanMillC m Timereport
    , MonadPlanMillC m Team
    , MonadPlanMillC m User
    , MonadPlanMillC m UserCapacity
    , ForallFSymbol (MonadPlanMillC m) EnumDesc
    , MonadPlanMillC m AllRevenues2
    , MonadPlanMillC m ValueCreationByMonth
    , MonadPlanMillC m PersonValueCreation
    )
  => MonadPlanMillConstraint m where

    -- | Different planmill monads have different constraints
    type MonadPlanMillC m :: * -> Constraint

    -- | We need vector constraints too
    entailMonadPlanMillCVector
        :: Proxy m -> Proxy a
        -> MonadPlanMillC m a :- MonadPlanMillC m (Vector a)

-------------------------------------------------------------------------------
-- MonadPlanMill
-------------------------------------------------------------------------------

-- | Class of monads capable to do planmill operations.
class MonadPlanMillConstraint m => MonadPlanMill m where
    -- | "Lift" planmill actions to monad action
    planmillAction :: MonadPlanMillC m a => PlanMill a -> m a
--
-- | Use this for actions retutning @'Vector' a@
planmillVectorAction
    :: forall m a. (MonadPlanMill m, MonadPlanMillC m a)
    => PlanMill (Vector a) -> m (Vector a)
planmillVectorAction = planmillAction \\  -- hello CPP
    entailMonadPlanMillCVector (Proxy :: Proxy m) (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- MonadPlanMillQuery
-------------------------------------------------------------------------------

-- | Class of monads capable to do planmill queries (i.e. serialisable
-- read-only operations).
class (MonadPlanMillConstraint m, MonadMemoize m) => MonadPlanMillQuery m where
    -- | "Lift" planmill queries to monad action
    planmillQuery :: MonadPlanMillC m a => Query a -> m a

planmillVectorQuery
    :: forall m a. (MonadPlanMillQuery m, MonadPlanMillC m a)
    => Query (Vector a) -> m (Vector a)
planmillVectorQuery = planmillQuery \\  -- hello CPP
    entailMonadPlanMillCVector (Proxy :: Proxy m) (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- Simple instance for "IO"
-------------------------------------------------------------------------------

instance Monad m
    => MonadPlanMillConstraint (ReaderT env m)
  where
    type MonadPlanMillC (ReaderT env m) = FromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict

instance (MonadIO m, HasPlanMillCfg env)
    => MonadPlanMill (ReaderT env m)
  where
    planmillAction planmill = do
        cfg <- view planmillCfg
        liftIO $ evalPlanMillIO cfg planmill

-------------------------------------------------------------------------------
-- Fancier instance for "PureT"
-------------------------------------------------------------------------------

instance Monad m => MonadPlanMillConstraint (PureT e r m) where
    type MonadPlanMillC (PureT e r m) = FromJSON
    entailMonadPlanMillCVector _ _    = Sub Dict

instance
    ( Monad m, MonadIO m, MonadBaseControl IO m
    , MonadThrow m
    , MonadTime m, MonadClock m
    , ContainsCryptoGenError e, HasHttpManager r, HasCryptoPool r, HasLoggerEnv r
    , HasPlanMillCfg r
    )
    => MonadPlanMill (PureT e r m)
  where
    planmillAction = evalPlanMill
