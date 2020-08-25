{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Okta.Request where

import Data.Constraint  (Dict (..))
import Futurice.Prelude
import Prelude ()

import Okta.Types

data Req a where
    ReqGetAllUsers         :: Req [User]
    ReqGetAllGroups        :: Req [Group]
    ReqGetGroupUsers       :: OktaGroupId -> Req [User]
    ReqGetAllApps          :: Req [App]
    ReqGetAppUsers         :: OktaAppId -> Req [AppUser]
    ReqCreateUser          :: NewUser -> Req User
    ReqUpdateUser          :: OktaId -> Value -> Req User
    ReqAddUserToGroup      :: OktaGroupId -> OktaId -> Req ()
    ReqRemoveUserFromGroup :: OktaGroupId -> OktaId -> Req ()

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqGetAllUsers               = hashWithSalt salt (0 :: Int)
    hashWithSalt salt ReqGetAllGroups              = hashWithSalt salt (1 :: Int)
    hashWithSalt salt (ReqGetGroupUsers t)         = hashWithSalt salt (2 :: Int, t)
    hashWithSalt salt ReqGetAllApps                = hashWithSalt salt (3 :: Int)
    hashWithSalt salt (ReqGetAppUsers t)           = hashWithSalt salt (4 :: Int, t)
    hashWithSalt salt (ReqCreateUser t)            = hashWithSalt salt (5 :: Int, t)
    hashWithSalt salt (ReqUpdateUser u t)          = hashWithSalt salt (6 :: Int, u, t)
    hashWithSalt salt (ReqAddUserToGroup u g)      = hashWithSalt salt (7 :: Int, u, g)
    hashWithSalt salt (ReqRemoveUserFromGroup u g) = hashWithSalt salt (8 :: Int, u, g)

requestDict
    :: ( c [User]
       , c [Group]
       , c [User]
       , c [App]
       , c [AppUser]
       , c User
       , c ())
    => Proxy c
    -> Req a
    -> Dict (c a)
requestDict _ ReqGetAllUsers = Dict
requestDict _ ReqGetAllGroups = Dict
requestDict _ (ReqGetGroupUsers _) = Dict
requestDict _ ReqGetAllApps = Dict
requestDict _ (ReqGetAppUsers _) = Dict
requestDict _ (ReqCreateUser _) = Dict
requestDict _ (ReqUpdateUser _ _) = Dict
requestDict _ (ReqAddUserToGroup _ _) = Dict
requestDict _ (ReqRemoveUserFromGroup _ _) = Dict
