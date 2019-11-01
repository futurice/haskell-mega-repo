{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Okta.Request where

import Data.Constraint  (Dict (..))
import Futurice.Prelude
import Prelude ()

import Okta.Types

data Req a where
    ReqGetAllUsers   :: Req [User]
    ReqGetAllGroups  :: Req [Group]
    ReqGetGroupUsers :: Text -> Req [User]
    ReqGetAllApps    :: Req [App]
    ReqGetAppUsers   :: Text -> Req [AppUser]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqGetAllUsers       = hashWithSalt salt (0 :: Int)
    hashWithSalt salt ReqGetAllGroups      = hashWithSalt salt (1 :: Int)
    hashWithSalt salt (ReqGetGroupUsers t) = hashWithSalt salt (2 :: Int, t)
    hashWithSalt salt ReqGetAllApps        = hashWithSalt salt (3 :: Int)
    hashWithSalt salt (ReqGetAppUsers t)   = hashWithSalt salt (4 :: Int, t)

requestDict
    :: ( c [User]
       , c [Group]
       , c [User]
       , c [App]
       , c [AppUser])
    => Proxy c
    -> Req a
    -> Dict (c a)
requestDict _ ReqGetAllUsers = Dict
requestDict _ ReqGetAllGroups = Dict
requestDict _ (ReqGetGroupUsers _) = Dict
requestDict _ ReqGetAllApps = Dict
requestDict _ (ReqGetAppUsers _) = Dict
