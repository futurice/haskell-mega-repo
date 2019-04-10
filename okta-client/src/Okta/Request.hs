{-# LANGUAGE GADTs #-}
module Okta.Request where

import Okta.Types

data Req a where
    ReqGetAllUsers :: Req [User]
