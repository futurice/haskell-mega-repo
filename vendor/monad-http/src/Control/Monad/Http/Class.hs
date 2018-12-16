{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Control.Monad.Http.Class (
    MonadHttp(..),
    MonadStreamingHttp(..),
    BodyReaderM,
    defaultHttpLbs,
    brConsume,
) where

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class    (MonadIO (..))
import Control.Monad.Operational (ProgramT, singleton)

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client  as H

import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.Except   (ExceptT (..), runExceptT)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe    (MaybeT (..))

import Control.Monad.Trans.Error  (Error, ErrorT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.RWS    (RWST (..))
import Control.Monad.Trans.State  (StateT (..))
import Control.Monad.Trans.Writer (WriterT (..))
import Log.Monad                  (LogT (..))

import qualified Control.Monad.Trans.RWS.Strict    as Strict (RWST (..))
import qualified Control.Monad.Trans.State.Strict  as Strict (StateT (..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT (..))

import Control.Monad.CryptoRandom (CRandT (..))

import Control.Monad.Logger (LoggingT (..), NoLoggingT (..))
import Control.Monad.Random (RandT, liftRandT, runRandT)

#if !MIN_VERSION_MonadRandom(0, 4, 0)
import Control.Monad.Random (RandomGen)
#endif

import Control.Monad.Http.Operational (HttpInstr (..))
import Control.Monad.Trans.Http       (HttpT (..), liftHttpT)

type BodyReaderM m = m S.ByteString

------------------------------------------------------------------------------
-- MonadHttp
------------------------------------------------------------------------------

-- | The monad capable to do HTTP requests.
class
#if MIN_VERSION_base(4,8,0)
  Monad m
#else
  (Applicative m, Monad m)
#endif
  => MonadHttp m where
    -- | A function which perform request,s reads the whole response body
    -- and immediately releases resources.
    httpLbs :: H.Request -> m (H.Response L.ByteString)

instance MonadHttp m => MonadHttp (IdentityT m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (ReaderT r m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (StateT r m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (Strict.StateT r m) where httpLbs = lift . httpLbs
instance (MonadHttp m, Monoid w) => MonadHttp (WriterT w m) where httpLbs = lift . httpLbs
instance (MonadHttp m, Monoid w) => MonadHttp (Strict.WriterT w m) where httpLbs = lift . httpLbs
instance (MonadHttp m, Monoid w) => MonadHttp (RWST r w s m) where httpLbs = lift . httpLbs
instance (MonadHttp m, Monoid w) => MonadHttp (Strict.RWST r w s m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (MaybeT m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (ExceptT e m) where httpLbs = lift . httpLbs
instance (MonadHttp m, Error e) => MonadHttp (ErrorT e m) where httpLbs = lift . httpLbs

instance
#if MIN_VERSION_MonadRandom(0, 4, 0)
  MonadHttp m
#else
  (MonadHttp m, RandomGen g)
#endif
  => MonadHttp (RandT g m) where httpLbs = lift . httpLbs

instance
#if MIN_VERSION_monadcryptorandom(0, 7, 0)
   MonadHttp m
#else
  (MonadHttp m, Error e)
#endif
  => MonadHttp (CRandT g e m) where httpLbs = lift . httpLbs

instance MonadHttp m => MonadHttp (LoggingT m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (NoLoggingT m) where httpLbs = lift . httpLbs
instance MonadHttp m => MonadHttp (LogT m) where httpLbs = lift . httpLbs

------------------------------------------------------------------------------
-- MonadStreamingHttp
------------------------------------------------------------------------------

-- | The monad capable to do streaming HTTP requests.
class MonadHttp m => MonadStreamingHttp m where
    withResponse :: H.Request -> (H.Response (BodyReaderM m) -> m a) -> m a

    -- ^ Get a single chunk of data from the response body, or an empty
    -- bytestring if no more data is available.
    --
    -- Note that in order to consume the entire request body, you will need to
    -- repeatedly call this function until you receive an empty @ByteString@ as
    -- a result.
    brRead :: BodyReaderM m -> m S.ByteString
    brRead = id

-- like in https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/src/Control-Monad-Catch.html#instance%20MonadThrow%20(IdentityT%20m)
instance MonadStreamingHttp m => MonadStreamingHttp (IdentityT m) where
    withResponse req f = lift $ withResponse req (runIdentityT . f . fmap lift)

instance MonadStreamingHttp m => MonadStreamingHttp (ReaderT r m) where
    withResponse req f =
        ReaderT $ \r ->
            withResponse req $ \res ->
                runReaderT (f $ fmap lift res) r

instance MonadStreamingHttp m => MonadStreamingHttp (StateT r m) where
    withResponse req f =
        StateT $ \s ->
            withResponse req $ \res ->
                runStateT (f $ fmap lift res) s

instance MonadStreamingHttp m => MonadStreamingHttp (Strict.StateT r m) where
    withResponse req f =
        Strict.StateT $ \s ->
            withResponse req $ \res ->
                Strict.runStateT (f $ fmap lift res) s

instance (MonadStreamingHttp m, Monoid w) => MonadStreamingHttp (WriterT w m) where
    withResponse req f =
        WriterT $ withResponse req $ \res ->
            runWriterT (f $ fmap lift res)

instance (MonadStreamingHttp m, Monoid w) => MonadStreamingHttp (Strict.WriterT w m) where
    withResponse req f =
        Strict.WriterT $ withResponse req $ \res ->
            Strict.runWriterT (f $ fmap lift res)

instance (MonadStreamingHttp m, Monoid w) => MonadStreamingHttp (RWST r w s m) where
    withResponse req f =
        RWST $ \r s ->
            withResponse req $ \res ->
                runRWST (f $ fmap lift res) r s

instance (MonadStreamingHttp m, Monoid w) => MonadStreamingHttp (Strict.RWST r w s m) where
    withResponse req f =
        Strict.RWST $ \r s ->
            withResponse req $ \res ->
                Strict.runRWST (f $ fmap lift res) r s

instance MonadStreamingHttp m => MonadStreamingHttp (MaybeT m) where
    withResponse req f =
        MaybeT $ withResponse req $ \res ->
            runMaybeT (f $ fmap lift res)

instance MonadStreamingHttp m => MonadStreamingHttp (ExceptT e m) where
    withResponse req f =
        ExceptT $ withResponse req $ \res ->
            runExceptT (f $ fmap lift res)

instance (MonadStreamingHttp m, Error e) => MonadStreamingHttp (ErrorT e m) where
    withResponse req f =
        ErrorT $ withResponse req $ \res ->
            runErrorT (f $ fmap lift res)

instance
#if MIN_VERSION_MonadRandom(0, 4, 0)
  MonadStreamingHttp m
#else
  (MonadStreamingHttp m, RandomGen g)
#endif
  => MonadStreamingHttp (RandT g m) where
    withResponse req f =
        liftRandT $ \r ->
            withResponse req $ \res ->
                runRandT (f $ fmap lift res) r

instance
#if MIN_VERSION_monadcryptorandom(0, 7, 0)
   MonadStreamingHttp m
#else
  (MonadStreamingHttp m, Error e)
#endif
  => MonadStreamingHttp (CRandT g e m) where
    withResponse req f =
        CRandT $ withResponse req $ \res ->
            unCRandT (f $ fmap CRandT res)

instance MonadStreamingHttp m => MonadStreamingHttp (LoggingT m) where
    withResponse req f =
        LoggingT $ \r ->
            withResponse req $ \res ->
                runLoggingT (f $ fmap lift res) r

instance MonadStreamingHttp m => MonadStreamingHttp (NoLoggingT m) where
    withResponse req f = lift $ withResponse req (runNoLoggingT . f . fmap lift)

------------------------------------------------------------------------------
-- Convenience functions
------------------------------------------------------------------------------

-- | A convenience wrapper around 'withResponse' which reads in the entire
-- response body and immediately releases resources.
defaultHttpLbs :: MonadStreamingHttp m => H.Request -> m (H.Response L.ByteString)
defaultHttpLbs req = withResponse req $ \res -> do
    bss <- brConsume $ H.responseBody res
    return res { H.responseBody = L.fromChunks bss }

-- | Strictly consume all remaining chunks of data from the stream.
brConsume :: MonadStreamingHttp m => BodyReaderM m -> m [S.ByteString]
brConsume brRead' =
    go id
  where
    go front = do
        x <- brRead'
        if S.null x
            then return $ front []
            else go (front . (x:))

------------------------------------------------------------------------------
-- HttpT
------------------------------------------------------------------------------

instance
#if MIN_VERSION_base(4,8,0)
  MonadIO m
#else
  (Applicative m, MonadIO m)
#endif
  => MonadHttp (HttpT m) where
    httpLbs req = HttpT (liftIO . H.httpLbs req)

-- | /TODO:/ Generalise to MonadIO + MonadMask?
instance m ~ IO => MonadStreamingHttp (HttpT m) where
    withResponse req f = HttpT (\mgr -> H.withResponse req mgr (flip runHttpT mgr . f . fmap liftHttpT))

------------------------------------------------------------------------------
-- HttpOpT
------------------------------------------------------------------------------

-- | 'HttpOpT'
instance Monad m => MonadHttp (ProgramT HttpInstr m) where
    httpLbs = singleton . HttpLbs
