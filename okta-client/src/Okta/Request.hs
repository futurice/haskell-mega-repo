{-# LANGUAGE GADTs #-}
module Okta.Request where

import Futurice.Prelude
import Prelude ()

import Okta.Types

data Req a where
    ReqGetAllUsers   :: Req [User]
    ReqGetAllGroups  :: Req [Group]
    ReqGetGroupUsers :: Text -> Req [User]
