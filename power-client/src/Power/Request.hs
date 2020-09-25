{-# LANGUAGE GADTs #-}
module Power.Request where

import Futurice.Prelude
import Prelude ()

import Power.Types

data Req a where
    ReqPeople         :: Req [Person]
    ReqAllocation     :: Maybe Day -> Maybe Day -> Req [Allocation]
    ReqCustomer       :: Req [Customer]
    ReqProject        :: Req [Project]
    ReqProjectMapping :: Req [ProjectMapping]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqPeople           = salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt (ReqAllocation a b) = salt `hashWithSalt` (1 :: Int, a, b)
    hashWithSalt salt ReqCustomer         = salt `hashWithSalt` (2 :: Int)
    hashWithSalt salt ReqProject          = salt `hashWithSalt` (3 :: Int)
    hashWithSalt salt ReqProjectMapping   = salt `hashWithSalt` (4 :: Int)
