module Futurice.App.ProxyMgmt.Types where

import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Futurice.Prelude
import Prelude ()

data Token = Token
    { tUsername   :: !Text
    , tActive     :: !Bool
    , tUsertype   :: !Text
    , tPolicyName :: !Text
    }
  deriving (Show, Generic)

instance NFData Token
instance FromRow Token where
    fromRow = Token <$> field <*> field <*> field <*> field

data AccessEntry = AccessEntry
    { aeUser     :: !Text
    , aeStamp    :: !UTCTime
    , aeEndpoint :: !Text
    }
  deriving (Show, Generic)

instance NFData AccessEntry
instance FromRow AccessEntry where
    fromRow = AccessEntry <$> field <*> field <*> field
