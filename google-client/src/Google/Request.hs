{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Google.Request (
    Req (..),
    CalendarResource,
    Event,
    ReadOnlyScope (..),
    requestDict) where

import Futurice.Prelude
import Prelude ()

import Data.Constraint                (Dict (..))
import Google.Types
import Network.Google.AppsCalendar
import Network.Google.Directory.Types

data ReadOnlyScope = ReadOnly
                   | AlsoWriteAccess
                   deriving (Eq, Show, Hashable, Generic)

data Req a where
    ReqCalendarResources         :: ReadOnlyScope -> Req [CalendarResource]
    ReqEvents                    :: ReadOnlyScope -> Day -> Day -> Text -> Req [Event]
    ReqInvite                    :: CalendarEvent -> Req Event
    ReqDeleteEvent               :: Text -> Req ()
    ReqPatchEvent                :: Text -> CalendarEvent -> Req Event

deriving instance Eq (Req a)
deriving instance Show (Req a)

instance Hashable (Req a) where
    hashWithSalt salt (ReqCalendarResources readOnly) = hashWithSalt salt (0::Int, readOnly)
    hashWithSalt salt (ReqEvents readOnly startDay endDay email) = hashWithSalt salt (1::Int,readOnly,startDay,endDay,email)
    hashWithSalt salt (ReqInvite ev) = hashWithSalt salt (2::Int, ev)
    hashWithSalt salt (ReqDeleteEvent eventId) = hashWithSalt salt (3::Int, eventId)
    hashWithSalt salt (ReqPatchEvent eventId ev) = hashWithSalt salt (4::Int, eventId, ev)

requestDict
    :: ( c [CalendarResource]
       , c [Event]
       , c Event
       , c ())
    => Proxy c
    -> Req a
    -> Dict (c a)
requestDict _ (ReqEvents _ _ _ _)      = Dict
requestDict _ (ReqCalendarResources _) = Dict
requestDict _ (ReqInvite _)            = Dict
requestDict _ (ReqDeleteEvent _)       = Dict
requestDict _ (ReqPatchEvent _ _)      = Dict
