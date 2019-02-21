{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Schedule.Types where

import Futurice.IdMap   (HasKey, Key, key)
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

data SchedulingRequestStatus = Accepted
                             | Declined -- TODO: check what are the actual statuses
                             deriving Show

data SchedulingRequest = SchedulingRequest
    { _srId             :: !Int -- TODO: change this to proper id type like UUID
    , _srJson           :: !Text -- TODO: separate to it's own type
    , _srRequestedAt    :: !UTCTime -- should this be localtime?
    , _srStatus         :: !SchedulingRequestStatus
    , _srError          :: !Text -- this should not probably be it's own field?
    , _srRequestedBy    :: !P.EmployeeId
    , _srPdfUrl         :: !Text  -- what was this
    } deriving Show

makeLenses ''SchedulingRequest

instance HasKey SchedulingRequest where
    type Key SchedulingRequest = Int
    key = srId
