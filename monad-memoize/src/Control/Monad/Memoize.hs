module Control.Monad.Memoize where

import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Hashable              (Hashable)
import Data.Typeable              (Typeable)

import qualified Haxl.Core as Haxl

class Monad m => MonadMemoize m where
    memo :: (Typeable a, Typeable k, Hashable k, Eq k) => k -> m a -> m a

{-
    memoize1
        :: (Eq a, Hashable a)
        => (a -> m b)
        -> m (a ->  m b)
    memoize :: m a -> m (m a)

    memoize m = fmap ($ ()) (memoize1 (const m))

    memoize2
        :: (Eq a, Hashable a, Eq b, Hashable b)
        => (a -> b -> m c)
        -> m (a -> b -> m c)
    memoize2 f = curry <$> memoize1 (uncurry f)
-}

-- | We assume that environment doesn't change
-- or at least doesn't change considerably to affect memoized computations
instance MonadMemoize m => MonadMemoize (ReaderT env m) where
    memo k (ReaderT m) = ReaderT $ \env -> memo k (m env)

instance MonadMemoize (Haxl.GenHaxl u) where
    memo = Haxl.memo
