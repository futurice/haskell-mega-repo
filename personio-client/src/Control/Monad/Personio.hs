{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Control.Monad.Personio (
    MonadPersonio (..),
    ) where

import Futurice.Prelude
import Prelude ()
import Data.Aeson.Compat (FromJSON)
import Data.Constraint (Constraint)

import Personio.Request
import Personio.Types

class
    ( Monad m
    , MonadPersonioC m [Employee]
    , MonadPersonioC m (Map Day [SimpleEmployee])
    ) => MonadPersonio m
  where
    type MonadPersonioC m a :: Constraint
    type MonadPersonioC m a = FromJSON a

    personio :: (MonadPersonioC m a) => PersonioReq a -> m a
