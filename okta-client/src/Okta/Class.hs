module Okta.Class where

import Futurice.Prelude
import Prelude ()

import Okta.Request
import Okta.Types

class Monad m => MonadOkta m where
    oktaReq :: Req a -> m a

users :: MonadOkta m => m [User]
users = oktaReq ReqGetAllUsers

appUsers :: MonadOkta m => OktaAppId -> m [AppUser]
appUsers = oktaReq . ReqGetAppUsers

updateUser :: MonadOkta m => OktaId -> Value -> m User
updateUser oktaid value = oktaReq $ ReqUpdateUser oktaid value

createUser :: MonadOkta m => NewUser -> m User
createUser = oktaReq . ReqCreateUser

groups :: MonadOkta m => m [Group]
groups = oktaReq ReqGetAllGroups

groupMembers :: MonadOkta m => OktaGroupId -> m [User]
groupMembers = oktaReq . ReqGetGroupUsers

addUserToGroup :: MonadOkta m => OktaGroupId -> OktaId -> m ()
addUserToGroup gid uid = oktaReq $ ReqAddUserToGroup gid uid

deleteUserFromGroup :: MonadOkta m => OktaGroupId -> OktaId -> m ()
deleteUserFromGroup gid uid = oktaReq $ ReqRemoveUserFromGroup gid uid
