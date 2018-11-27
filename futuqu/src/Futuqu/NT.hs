{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Futuqu.NT where

import Futurice.Prelude
import Prelude ()

type m ~> n = forall a k. (Typeable a, NFData a, Eq k, Hashable k, Typeable k) => Maybe k -> m a -> n a
newtype m :~> n = NT (m ~> n)
