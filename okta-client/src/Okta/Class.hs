module Okta.Class where

import Futurice.Prelude
import Prelude ()

import Okta.Request
import Okta.Types

class Monad m => MonadOkta m where
    oktaReq :: Req a -> m a

users :: MonadOkta m => m [User]
users = oktaReq ReqGetAllUsers

appUsers :: MonadOkta m => Text -> m [AppUser]
appUsers = oktaReq . ReqGetAppUsers

updateUser :: MonadOkta m => OktaId -> Value -> m User
updateUser oktaid value = oktaReq $ ReqUpdateUser oktaid value

createUser :: MonadOkta m => NewUser -> m User
createUser = oktaReq . ReqCreateUser

groups :: MonadOkta m => m [Group]
groups = oktaReq ReqGetAllGroups
