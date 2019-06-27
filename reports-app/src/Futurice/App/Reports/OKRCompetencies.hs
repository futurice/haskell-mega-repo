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

import Futuqu.Rada.People           (peopleData, Person(..))
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Tribe               (Tribe, defaultTribe, tribeToText)
import PlanMill.Types.Project
import Prelude ()

import qualified Data.Map.Strict  as Map
import qualified Data.List        as L
import qualified Data.Set         as S
import qualified Data.Vector      as V
import qualified FUM.Types.Login  as FUM
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ
import Numeric.Interval.NonEmpty            (Interval, (...), member, sup)


-- Data types

data ProjectCompetences = ProjectCompetences
    { pcProjectName :: Text
    , pcNumberOfCompetences :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
  
deriveGeneric ''ProjectCompetences
instance ToSchema ProjectCompetences where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ProjectCompetences   `Via` Sopica ProjectCompetences |]
deriveVia [t| FromJSON ProjectCompetences `Via` Sopica ProjectCompetences |]

newtype CompetencyReport = CR (Map Tribe [ProjectCompetences])
    deriving newtype (Show, NFData, ToJSON, FromJSON) 

deriveGeneric ''CompetencyReport

instance ToSchema CompetencyReport where declareNamedSchema = newtypeDeclareNamedSchema

-- Data query

ongoingProject :: Interval Day -> Project -> Bool
ongoingProject interval p =
    case pFinish p of
        Nothing -> False

        Just d ->
            member (utctDay d) interval || (utctDay d > sup interval)

findTribeForProject :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m) => Project -> m (Tribe, Project)
findTribeForProject project = do
    let uid = pProjectManager project
    ppl <- peopleData
    let person = listToMaybe $ filter (\p -> pPlanmill p == uid) ppl
    let tribe = fromMaybe defaultTribe $ fmap pTribe person
    return (tribe, project)

findAssignmentsForProject :: forall m. (MonadPlanMillQuery m) => (Tribe, Project) -> m (Tribe, Text, PM.Assignments)
findAssignmentsForProject (tribe, project) = do
    let pid = _pId project
    assignments <- PMQ.assignments pid
    return (tribe, PM.pName project, assignments)

findUserForAssignment :: forall m. (MonadPlanMillQuery m) => (Tribe, Text, PM.Assignments) -> m (Tribe, Text, [PM.User])
findUserForAssignment (tribe, project, assignments) = do
    users <- mapM getUser $ V.map PM.aPersonOrTeam assignments
    let cleaned = catMaybes $ V.toList users
    return (tribe, project, cleaned)
    where
        getUser :: forall n. (MonadPlanMillQuery n) => (Either PM.UserId PM.TeamId) -> n (Maybe PM.User)
        getUser (Left uid) = do
            user <- PMQ.user uid
            return $ Just user
        getUser _ = do
            return Nothing

competencyData :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m) => m CompetencyReport
competencyData = do
    today <- currentDay
    let interval = beginningOfCurrMonth today ... pred today

    -- get projects
    projects <- PMQ.projects

    -- filter projects by time interval (finish date during or after interval)
    let projectsForInterval = V.filter (ongoingProject interval) projects

    -- map tribe to project by looking up team of project manager
    withTribe <- mapM findTribeForProject projectsForInterval

    -- get assignments for each project
    withAssignments <- mapM findAssignmentsForProject withTribe

    -- map assigned users to Planmill user
    toUsers <- mapM findUserForAssignment withAssignments

    -- flatmap project users to competencies and count uniques
    let withCompetences = V.map (\(t, p, u) -> (t, p, S.size . S.fromList . catMaybes $ map PM.uCompetence u)) toUsers
    
    -- group by tribe
    let grouped = L.groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2) $ V.toList withCompetences

    -- Return as type
    let rawReport = map (\((t, p, u):xs) -> (t, (ProjectCompetences p u : map (\(_, p', u') -> ProjectCompetences p' u') xs))) grouped
    let report = Map.fromList rawReport

    return $ CR report

-- HTML table

instance ToHtml CompetencyReport where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderCompetencyReport

renderCompetencyReport :: CompetencyReport -> HtmlPage "okr-competences"
renderCompetencyReport (CR report) = page_ "OKR Competence Report" $ do
    h1_ "OKR Competences"

    
