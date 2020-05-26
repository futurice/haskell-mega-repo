module Peakon.Class where

import Futurice.Prelude
import Prelude ()

import Peakon.Request

class Monad m => MonadPeakon m where
    peakonReq :: Req a -> m a

engagementOverview :: MonadPeakon m => m Value
engagementOverview = peakonReq ReqEngagementOverview

engagementDrivers :: MonadPeakon m => m Value
engagementDrivers = peakonReq ReqEngagementDrivers

segments :: MonadPeakon m => m Value
segments = peakonReq ReqSegments