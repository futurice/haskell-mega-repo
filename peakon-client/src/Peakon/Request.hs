{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Peakon.Request where

import Data.Constraint  (Dict (..))
import Futurice.Prelude
import Prelude ()

import Peakon.Types

data Req a where
    ReqEngagementOverview :: Req Value
    ReqEngagementDrivers  :: Req Value
    ReqSegments           :: Req Value
    ReqEmployees          :: Req [Employee]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqEngagementOverview    = hashWithSalt salt (0 :: Int)
    hashWithSalt salt ReqEngagementDrivers     = hashWithSalt salt (1 :: Int)
    hashWithSalt salt ReqSegments              = hashWithSalt salt (2 :: Int)
    hashWithSalt salt ReqEmployees             = hashWithSalt salt (3 :: Int)

requestDict
    ::(c Value,
      c [Employee])
    => Proxy c
    -> Req a
    -> Dict (c a)
requestDict _ ReqEngagementOverview = Dict
requestDict _ ReqEngagementDrivers  = Dict
requestDict _ ReqSegments           = Dict
requestDict _ ReqEmployees          = Dict
