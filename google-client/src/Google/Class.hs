module Google.Class where

import Futurice.Prelude
import Prelude ()

import Google.Request

class Monad m => MonadGoogle m where
    googleReq :: Req a -> m a

googleCalendarResources :: (MonadGoogle m) => ReadOnlyScope -> m [CalendarResource]
googleCalendarResources readonly = googleReq (ReqCalendarResources readonly)

googleCalendarEvents :: MonadGoogle m => ReadOnlyScope -> Day -> Day -> Text -> m [Event]
googleCalendarEvents readonly startDay endDay email = googleReq (ReqEvents readonly startDay endDay email)
