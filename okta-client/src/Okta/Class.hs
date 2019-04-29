module Okta.Class where

import Futurice.Prelude
import Prelude ()

import Okta.Request

class Monad m => MonadOkta m where
    oktaReq :: Req a -> m a
