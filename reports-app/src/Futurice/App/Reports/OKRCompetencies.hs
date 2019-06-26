{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.OKRCompetencies where

import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import PlanMill.Types.Project
import Prelude ()

import qualified Data.Vector      as DV
import qualified FUM.Types.Login  as FUM
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ
import Numeric.Interval.NonEmpty            (Interval, (...), member, sup)


-- Data types

data CompetencyReport = CompetencyReport
    { crTribe :: String
    , crProjects :: [(String, Int)] -- Project name, # of competencies
    }

-- Data query

ongoingProject :: Interval Day -> Project -> Bool
ongoingProject interval p =
    case pFinish p of
        Nothing -> False

        Just d ->
            member (utctDay d) interval || (utctDay d > sup interval)

competencyData :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m) => m CompetencyReport
competencyData = do
    today <- currentDay
    let interval = beginningOfCurrMonth today ... pred today

    -- get projects
    projects <- PMQ.projects

    -- filter projects by time interval (finish date during or after interval)
    let projectsForInterval = DV.filter (ongoingProject interval) projects

    -- map tribe to project by looking up team of project manager

    -- get assignments for each project

    -- map assigned users to Personio user

    -- flatmap project users to competencies 

    -- count unique competencies per project

    -- group by tribe

    return $ CompetencyReport "" []

