{-# LANGUAGE GADTs #-}
module Google.Request (
    Req (..),
    CalendarResource,
    Event) where

import Futurice.Prelude
import Prelude ()

import Network.Google.AppsCalendar
import Network.Google.Directory.Types

data Req a where
    ReqCalendarResources :: Req [CalendarResource]
    ReqEvents            :: Day -> Day -> Text -> Req [Event]

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt ReqCalendarResources = hashWithSalt salt (0::Int)
    hashWithSalt salt (ReqEvents startDay endDay email) = hashWithSalt salt (1::Int,startDay,endDay,email)
