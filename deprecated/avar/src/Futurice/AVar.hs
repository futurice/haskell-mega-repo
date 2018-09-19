{-# LANGUAGE Safe #-}
-- | 'AVar' is something between 'TVar' and 'TMVar'.
--
-- * 'TVar' has to be given a value on creation.
--
-- * 'TMVar' can be emptied during its lifetime.
--
-- * 'AVar' constructor can be given an @'Async' a@ value.
--
-- /Note:/ if the initial 'Async' fails, the 'AVar' will be left empty,
-- this should case large problems as 'readAVar' will block until the variable
-- is eventually filled.
module Futurice.AVar (
    -- * AVars
    AVar,
    newAVar,
    newEmptyAVar,
    newAVarIO,
    newEmptyAVarIO,
    readAVar,
    readAVarIO,
    writeAVar,
    writeAVarIO,
    tryReadAVar,
    swapAVar,
    tryPutAVar,
    isEmptyAVar,
    -- * Async combinators
    newAVarAsyncIO,
    asyncPutAVarIO,
    ) where

import Prelude.Compat
import Prelude ()

import Control.Concurrent.Async     (Async, async, waitSTM)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TMVar
import Data.Typeable                (Typeable)

-- | A 'AVar' is a synchronising variable, used
-- for communication between concurrent threads.  It can be thought of
-- as a box, which is empty at first, but can be filled once
--
-- It's a 'TMVar' which cannot be emptied, once filled. Value can be only reset.
newtype AVar a = AVar (TMVar a)
    deriving (Eq, Typeable)

-- |Create a 'AVar' which contains the supplied value.
newAVar :: a -> STM (AVar a)
newAVar a = AVar <$> newTMVar a

-- | @IO@ version of 'newAVar'.  This is useful for creating top-level
-- 'AVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newAVarIO :: a -> IO (AVar a)
newAVarIO a = AVar <$> newTMVarIO a

-- | Unsafe to use inside 'System.IO.Unsafe.unsafePerformIO'.
newAVarAsyncIO :: Async a -> IO (AVar a)
newAVarAsyncIO as = do
    avar <- newEmptyAVarIO
    _ <- asyncPutAVarIO avar as
    return avar

-- | Create a 'AVar' which is initially empty.
newEmptyAVar :: STM (AVar a)
newEmptyAVar = AVar <$> newEmptyTMVar

-- | @IO@ version of 'newEmptyAVar'.  This is useful for creating top-level
-- 'AVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newEmptyAVarIO :: IO (AVar a)
newEmptyAVarIO = AVar <$> newEmptyTMVarIO

-- | Put a value into a 'AVar'.  The 'tryPutAVar'
-- function attempts to put the value @a@ into the 'AVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutAVar :: AVar a -> a -> STM Bool
tryPutAVar (AVar t) = tryPutTMVar t

-- | Try to put value to 'AVar'
asyncPutAVarIO :: AVar a -> Async a -> IO (Async Bool)
asyncPutAVarIO avar as = async $ atomically $ do
    x <- waitSTM as
    tryPutAVar avar x

-- | Read a value of 'AVar'.
readAVar :: AVar a -> STM a
readAVar (AVar t) = readTMVar t

-- | @IO@ version on 'readAVar'.
readAVarIO :: AVar a -> IO a
readAVarIO = atomically . readAVar

-- | Write a value to 'AVar' replacing whatever value there was, if any.
writeAVar :: AVar a -> a -> STM ()
writeAVar (AVar tmvar) x = do
    _ <- tryTakeTMVar tmvar
    () <$ tryPutTMVar tmvar x

-- | @IO@ version of 'writeAVar'.
writeAVarIO :: AVar a -> a -> IO ()
writeAVarIO avar = atomically . writeAVar avar

-- | A version of 'readAVar' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadAVar :: AVar a -> STM (Maybe a)
tryReadAVar (AVar t) = tryReadTMVar t

-- | Swap the contents of a 'AVar' for a new value.
swapAVar :: AVar a -> a -> STM a
swapAVar (AVar t) = swapTMVar t

-- | Check whether a given 'AVar' is empty.
isEmptyAVar :: AVar a -> STM Bool
isEmptyAVar (AVar t) = isEmptyTMVar t
