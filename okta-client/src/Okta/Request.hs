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
    ReqGetAppUsers         :: OktaAppId -> Req [AppUser GithubProfile]
    ReqCreateUser          :: NewUser -> Req User
    ReqUpdateUser          :: OktaId -> Value -> Req User
    ReqAddUserToGroup      :: OktaGroupId -> OktaId -> Req ()
    ReqRemoveUserFromGroup :: OktaGroupId -> OktaId -> Req ()
    ReqGetAppLinks         :: OktaId -> Req [AppLink]
    ReqGetApplication      :: OktaAppId -> Req App
    ReqGetUser             :: OktaId -> Req User
    ReqGetSlackUsers       :: OktaAppId -> Req [AppUser SlackProfile]

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
    hashWithSalt salt (ReqGetAppLinks i)           = hashWithSalt salt (9 :: Int, i)
    hashWithSalt salt (ReqGetApplication i)        = hashWithSalt salt (10 :: Int, i)
    hashWithSalt salt (ReqGetUser u)               = hashWithSalt salt (11 :: Int, u)
    hashWithSalt salt (ReqGetSlackUsers u)         = hashWithSalt salt (12 :: Int, u)

requestDict
    :: ( c [User]
       , c [Group]
       , c [User]
       , c [App]
       , c App
       , c [AppUser SlackProfile]
       , c [AppUser GithubProfile]
       , c [AppLink]
       , c User
       , c ()
       , c Value)
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
requestDict _ (ReqGetAppLinks _) = Dict
requestDict _ (ReqGetApplication _) = Dict
requestDict _ (ReqGetUser _) = Dict
requestDict _ (ReqGetSlackUsers _) = Dict
