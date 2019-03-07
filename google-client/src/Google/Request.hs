{-# LANGUAGE GADTs #-}
module Google.Request (
    Req (..),
    CalendarResource,
    Event,
    ReadOnlyScope (..)) where

import Futurice.Prelude
import Prelude ()

import Network.Google.AppsCalendar
import Network.Google.Directory.Types

data ReadOnlyScope = ReadOnly
                   | AlsoWriteAccess
                   deriving (Eq, Show, Hashable, Generic)

data Req a where
    ReqCalendarResources         :: ReadOnlyScope -> Req [CalendarResource]
    ReqEvents                    :: ReadOnlyScope -> Day -> Day -> Text -> Req [Event]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt (ReqCalendarResources readOnly) = hashWithSalt salt (0::Int, readOnly)
    hashWithSalt salt (ReqEvents readOnly startDay endDay email) = hashWithSalt salt (1::Int,readOnly,startDay,endDay,email)
