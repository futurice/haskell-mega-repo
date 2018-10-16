{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HoursMock.World where

import Futurice.App.HoursApi.Class
import Futurice.App.HoursApi.Types     (EntryType (..))
import Futurice.App.HoursMock.MockData
import Futurice.Prelude
import Prelude ()
import Futurice.Integrations.TimereportKind (TimereportKind (..))

import qualified PlanMill as PM

newtype World = World
    { _worldTimereports :: [Timereport]
    }

makeLenses ''World

-- | What you get when you start the mock
cleanWorld :: UTCTime -> World
cleanWorld now = World
    { _worldTimereports =
        [ Timereport
            { _timereportId        = PM.Ident 1
            , _timereportTaskId    = taskDevelopment ^. taskId
            , _timereportProjectId = projectFoo ^. projectId
            , _timereportDay       = yesterday
            , _timereportComment   = "dev stuff"
            , _timereportAmount    = 7.5
            , _timereportKind      = KindBillable
            , _timereportClosed    = False
            }
        ]
    }
  where
    today = utctDay now
    yesterday = pred today
