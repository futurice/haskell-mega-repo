module Google.Class where

import Futurice.Prelude
import Prelude ()

import Google.Request

class Monad m => MonadGoogle m where
    googleReq :: Req a -> m a

googleCalendarResources :: MonadGoogle m => m [CalendarResource]
googleCalendarResources = googleReq ReqCalendarResources

googleCalendarEvents :: MonadGoogle m => Day -> Day -> Text -> m [Event]
googleCalendarEvents startDay endDay email = googleReq (ReqEvents startDay endDay email)
