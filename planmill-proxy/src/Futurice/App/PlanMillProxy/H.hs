{-# LANGUAGE TypeFamilies #-}
module Futurice.App.PlanMillProxy.H (
    H, runH,
    ) where

import Futurice.Prelude
import Prelude ()

import           Control.Monad.Logger      (LogLevel (LevelInfo))
import           Control.Monad.PlanMill
                 (MonadPlanMillConstraint (..), MonadPlanMillQuery (..))
import           Data.Constraint
import           Futurice.Constraint.Unit1 (Unit1)
import qualified Haxl.Core                 as H
import           PlanMill                  (Cfg)
import qualified PlanMill.Types.Query      as Q

import PlanMill.Queries.Haxl (initDataSourceSimpleIO)

newtype H a = H { unH :: H.GenHaxl () a }

instance Functor H where
    fmap f (H x) = H (fmap f x)

instance Applicative H where
    pure = H . pure
    H f <*> H x = H (f <*> x)
    H f *> H x = H (f *> x)

instance Monad H where
    return = pure
    (>>) = (*>)
    H f >>= k = H $ f >>= unH . k

instance MonadPlanMillConstraint H where
    type MonadPlanMillC H = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadPlanMillQuery H where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> H (H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

runH :: Cfg -> H a -> IO a
runH cfg (H haxl) = do
    let stateStore = H.stateSet (initDataSourceSimpleIO LevelInfo cfg) H.stateEmpty
    env <- H.initEnv stateStore ()
    H.runHaxl env haxl
