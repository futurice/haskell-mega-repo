{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Slack.Request where

import Data.Constraint  (Dict (..))
import Futurice.Prelude
import Prelude ()

import Slack.Types

data Req a where
    ReqSendMessage :: ChannelId -> Text -> Req ()
    ReqGetUsers    :: Req [User]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt (ReqSendMessage a b) = hashWithSalt salt (0 :: Int, a, b)
    hashWithSalt salt ReqGetUsers          = hashWithSalt salt (1 :: Int)

requestDict
    :: (c (),
       c [User])
       => Proxy c
       -> Req a
       -> Dict (c a)
requestDict _ (ReqSendMessage _ _ ) = Dict
requestDict _ ReqGetUsers           = Dict
