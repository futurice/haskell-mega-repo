{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoOverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE RankNTypes              #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
#endif
module Caching where

import PlanMill.Internal.Prelude
import Prelude ()

import Data.Binary          (encode)
import Data.Binary.Tagged   (taggedDecodeFileOrFail, taggedEncodeFile)
import Data.Constraint      (Dict (..), type (:-)(..), (\\))
import Data.Digest.Pure.SHA (sha256, showDigest)
import GHC.TypeLits         (KnownSymbol)

import Control.Monad.PlanMill
import PlanMill.Types

newtype CachingT m a = CachingT { runCachingT :: m a }

instance Functor m => Functor (CachingT m) where
    fmap f (CachingT x) = CachingT (fmap f x)

instance Applicative m => Applicative (CachingT m) where
    pure = CachingT . pure
    CachingT f <*> CachingT x = CachingT (f <*> x)
    CachingT x *> CachingT y = CachingT (x *> y)

instance (Applicative m, Monad m) => Monad (CachingT m) where
    return = pure
    (>>) = (*>)
    CachingT x >>= f = CachingT $ x >>= runCachingT . f

instance (Applicative m, MonadIO m) => MonadIO (CachingT m) where
    liftIO = CachingT . liftIO

instance (Applicative m, MonadThrow m) => MonadThrow (CachingT m) where
    throwM = CachingT . throwM

instance (Applicative m, MonadCatch m) => MonadCatch (CachingT m) where
    catch (CachingT a) handler =  CachingT $ catch a (runCachingT . handler)

instance MonadTrans CachingT where
    lift = CachingT

class (MonadPlanMillC m a, Binary a, HasStructuralInfo a, HasSemanticVersion a)
     => CachingTC m a

-- Unfortunate boilerplate 
instance MonadPlanMillC m Absence              => CachingTC m Absence
instance MonadPlanMillC m Account              => CachingTC m Account
instance MonadPlanMillC m Assignment           => CachingTC m Assignment
instance MonadPlanMillC m Me                   => CachingTC m Me
instance MonadPlanMillC m Meta                 => CachingTC m Meta
instance MonadPlanMillC m Project              => CachingTC m Project
instance MonadPlanMillC m ReportableAssignment => CachingTC m ReportableAssignment
instance MonadPlanMillC m Task                 => CachingTC m Task
instance MonadPlanMillC m Team                 => CachingTC m Team
instance MonadPlanMillC m TimeBalance          => CachingTC m TimeBalance
instance MonadPlanMillC m Timereport           => CachingTC m Timereport
instance MonadPlanMillC m User                 => CachingTC m User
instance MonadPlanMillC m UserCapacity         => CachingTC m UserCapacity
instance MonadPlanMillC m (EnumDesc s)         => CachingTC m (EnumDesc s)

instance (MonadPlanMillC m (Vector a), CachingTC m a) => CachingTC m (Vector a)

instance
    ForallFSymbol (MonadPlanMillC m) EnumDesc
    => ForallFSymbol (CachingTC m) EnumDesc
  where
    instFSymbol :: forall k. KnownSymbol k => Dict (CachingTC m (EnumDesc k))
    instFSymbol = case instFSymbol :: Dict (MonadPlanMillC m (EnumDesc k)) of
        Dict -> Dict
          
instance
    (Applicative m, MonadPlanMillConstraint m)
  => MonadPlanMillConstraint (CachingT m) where
    type MonadPlanMillC (CachingT m) = CachingTC m

    -- Nasty manual plumbing
    entailMonadPlanMillCVector
        :: forall a. Proxy (CachingT m) -> Proxy a
        -> MonadPlanMillC (CachingT m) a :- MonadPlanMillC (CachingT m) (Vector a)
    entailMonadPlanMillCVector _ p = Sub dict 
      where
        dict :: CachingTC m a => Dict (CachingTC m (Vector a))
        dict = Dict \\ entailMonadPlanMillCVector (Proxy :: Proxy m) p
            -- :: MonadPlanMillC m a :- MonadPlanMillC m (Vector a))

-- | Caches get requests
instance
    (Applicative m, MonadPlanMill m, MonadIO m, MonadCatch m)
  => MonadPlanMill (CachingT m) where
    planmillAction p@(PlanMillGet qs ps) =
        CachingT $ cached path $ planmillAction p
      where
        path = cacheFile ("get", qs, ps)

    planmillAction p@(PlanMillPagedGet qs ps) =
        CachingT $ cached path $ planmillAction p
      where
        path = cacheFile ("get-paged", qs, ps)

    planmillAction p@(PlanMillPost _ _) =
        CachingT $ planmillAction p

cacheFile :: Binary a => a -> FilePath
cacheFile = (".cache/" ++) .showDigest . sha256 . encode

cached :: ( Applicative m, MonadIO m, MonadCatch m
          , Binary a, HasStructuralInfo a, HasSemanticVersion a
          )
       => FilePath  -- ^ Cache file
       -> m a       -- ^ Action to perform on cache miss
       -> m a
cached path action = do
    e <- liftIO (taggedDecodeFileOrFail path) `catch` onIOError (Left undefined)
    case e of
        Right x -> return x
        Left _  -> do
            x <- action
            liftIO $ taggedEncodeFile path x
            pure x

onIOError :: Monad m => a -> IOError -> m a
onIOError x _ = return x
