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
