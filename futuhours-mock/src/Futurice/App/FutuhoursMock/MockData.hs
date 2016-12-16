{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.MockData (
  projects,
  days,
  internalProject,
  absenceProject,
  ) where
import Futurice.App.FutuhoursMock.Types
import Futurice.Prelude
import Prelude ()
import qualified PlanMill as PM

internalProject :: Project
internalProject = Project
      { _projectId=PM.Ident 1
      , _projectName="Internal Work" 
      , _projectTasks=[
           (mkTask 1 "Things") {_taskLatestEntry=mkLatestEntry "Doing things"}
         , (mkTask 2 "Stuff")  {_taskLatestEntry=mkLatestEntry "Doing stuff"}
         ]
      , _projectClosed=False
      }

absenceProject :: Project
absenceProject = Project
      { _projectId=PM.Ident 4
      , _projectName="Absences" 
      , _projectTasks=[
            (mkTask 6 "Balance leave") {_taskLatestEntry=mkLatestEntry "Balance leave"}
          , (mkTask 7 "Unpaid holiday") {_taskLatestEntry=mkLatestEntry "Unpaid holiday"}
          , (mkTask 8 "Sick leave") {_taskLatestEntry=mkLatestEntry "Sick leave"}
          ]
      , _projectClosed=False
      }

customerProject :: Project
customerProject = Project
    { _projectId=PM.Ident 2
    , _projectName="Actual customer work" 
    , _projectTasks=[
          (mkTask 3 "Development") {_taskLatestEntry=mkLatestEntry "Development"}
        , (mkTask 4 "Long weekend :()") {_taskLatestEntry=mkLatestEntry "On-Call"}
        ]
    , _projectClosed=False
    }

inactiveProject :: Project
inactiveProject = Project
    { _projectId=PM.Ident 3
    , _projectName="Not active project" 
    , _projectTasks=[
          (mkTask 5 "Doing work") { _taskLatestEntry=mkLatestEntry "Work" }
        , (mkTask 6 "Designing") { _taskLatestEntry=mkLatestEntry "Design" }
        ]
    , _projectClosed=True
    }

projects :: [Project]
projects = [
      internalProject
    , absenceProject
    , customerProject
    , inactiveProject]

days :: [HoursDay]
days = [
     mkHoursDay 
       { _dayHours=5
       , _dayEntries=[
              Entry { _entryId=PM.Ident 1
                    , _entryProjectId=internalProject ^. projectId
                    , _entryTaskId=fromMaybe (PM.Ident 1) (internalProject ^?  projectTasks . traverse . taskId)
                    , _entryDescription="Internal work"
                    , _entryHours=5
                    , _entryClosed=False
                    }
        ]
       }
    ,mkHoursDay
      { _dayHours=0.0
      , _dayHolidayName=Just "Public holiday"
      }
    ,mkHoursDay
         { _dayHours=0.0 }
    ,mkHoursDay 
         { _dayHours=0.0 }
    ,mkHoursDay 
         { _dayHours=0.0
         , _dayClosed=True
         }
    ,mkHoursDay
        { _dayHours=7.5
        , _dayEntries=[
            Entry { _entryId=PM.Ident 2
                  , _entryProjectId=absenceProject ^. projectId
                  , _entryTaskId=fromMaybe (PM.Ident 2) $ absenceProject ^?  projectTasks . traverse . taskId
                  , _entryDescription=fromMaybe "" $ absenceProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                  , _entryHours=7.5
                  , _entryClosed=False
                  }]
        }
    ,mkHoursDay
        { _dayHours=7.5
        , _dayClosed=True
        , _dayEntries=[
          Entry { _entryId=PM.Ident 3
                , _entryProjectId=absenceProject ^. projectId
                , _entryTaskId=fromMaybe (PM.Ident 3) $ absenceProject ^?  projectTasks . traverse . taskId
                , _entryDescription=fromMaybe "" $ absenceProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                , _entryHours=7.5
                , _entryClosed=False
                }]
        }
    ,mkHoursDay
        { _dayHours=10
        , _dayEntries=[
          Entry { _entryId=PM.Ident 13
                , _entryProjectId=customerProject ^. projectId
                , _entryTaskId=fromMaybe (PM.Ident 13) $ customerProject ^?  projectTasks . traverse . taskId
                , _entryDescription=fromMaybe "" $ customerProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                , _entryHours=10
                , _entryClosed=False
                }]
        }
    ,mkHoursDay
        { _dayHours=7.5
        , _dayEntries=[
            Entry { _entryId=PM.Ident 4
                  , _entryProjectId=absenceProject ^. projectId
                  , _entryTaskId=fromMaybe (PM.Ident 4) $ absenceProject ^?  projectTasks . traverse . taskId
                  , _entryDescription=fromMaybe "" $ absenceProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                  , _entryHours=2.5
                  , _entryClosed=False
                  }
          , Entry { _entryId=PM.Ident 5
                  , _entryProjectId=customerProject ^. projectId
                  , _entryTaskId=fromMaybe (PM.Ident 5) $ customerProject ^?  projectTasks . traverse . taskId
                  , _entryDescription="Customer work"
                  , _entryHours=5.0
                  , _entryClosed=False
                  }]
        }
    ,mkHoursDay
        { _dayHours=9.0
        , _dayEntries=[
          Entry { _entryId=PM.Ident 6
                , _entryProjectId=inactiveProject ^. projectId
                , _entryTaskId=fromMaybe (PM.Ident 6) $ inactiveProject ^?  projectTasks . traverse . taskId
                , _entryDescription=fromMaybe "" $ inactiveProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                , _entryHours=9.0
                , _entryClosed=True
                }]
        }
    ,mkHoursDay
        { _dayHours=7.5
        , _dayEntries=[
          Entry { _entryId=PM.Ident 7
                , _entryProjectId=inactiveProject ^. projectId
                , _entryTaskId=fromMaybe (PM.Ident 7) $ inactiveProject ^?  projectTasks . traverse . taskId
                , _entryDescription=fromMaybe "" $ inactiveProject ^?  projectTasks . traverse . taskLatestEntry . _Just . latestEntryDescription
                , _entryHours=7.5
                , _entryClosed=True
                }]
        }
  ]
