{-# LANGUAGE GADTs #-}
module Power.Request where

import Futurice.Prelude
import Prelude ()

import Power.Types

data Req a where
    ReqPeople :: Req [Person]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqPeople = salt
        `hashWithSalt` (0 :: Int)
