{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HoursMock.MockData where

import Futurice.Prelude
import Prelude ()

import Futurice.App.HoursApi.Class

import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Collections
-------------------------------------------------------------------------------

allProjects :: [Project]
allProjects =
    [ projectFoo
    , projectInternal
    ]

allTasks :: [Task]
allTasks =
    [ taskDevelopment
    , taskDesign
    , taskInternal
    ]

-------------------------------------------------------------------------------
-- Normal project
-------------------------------------------------------------------------------

projectFoo :: Project
projectFoo = Project
    { _projectId      = PM.Ident 1
    , _projectName    = "Foobar phase 303"
    , _projectClosed  = False
    , _projectAbsence = False
    }

taskDevelopment :: Task
taskDevelopment = Task
    { _taskId = PM.Ident 1
    , _taskName = "Development"
    , _taskProjectId = projectFoo ^. projectId
    , _taskFinish = $(mkDay "2019-12-31") -- in the future!
    }

taskDesign :: Task
taskDesign = Task
    { _taskId = PM.Ident 2
    , _taskName = "Design"
    , _taskProjectId = projectFoo ^. projectId
    , _taskFinish = $(mkDay "2019-12-31") -- in the future!
    }

-------------------------------------------------------------------------------
-- Internal project
-------------------------------------------------------------------------------

projectInternal :: Project
projectInternal = Project
    { _projectId      = PM.Ident 2
    , _projectName    = "Internal work"
    , _projectClosed  = False
    , _projectAbsence = False
    }

taskInternal :: Task
taskInternal = Task
    { _taskId = PM.Ident 3
    , _taskName = "Training"
    , _taskProjectId = projectInternal ^. projectId
    , _taskFinish = $(mkDay "2019-12-31") -- in the future!
    }

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

-- Add other projects as needed
