{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.OKRCompetencies where

import Futuqu.Rada.People                   (Person (..), peopleData)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Integrations.Common         (personioPlanmillMap)
import Futurice.Integrations.TimereportKind (TimereportKind (..), projectKind)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Tribe                       (Tribe, defaultTribe)
import PlanMill.Types.Project
import Prelude ()

import qualified Data.HashMap.Strict       as HM
import qualified Data.List                 as L
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified FUM.Types.Login           as FUM
import           Numeric.Interval.NonEmpty (Interval, member, sup, (...))
import qualified Personio                  as P
import qualified PlanMill                  as PM
import qualified PlanMill.Queries          as PMQ


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
        Nothing -> True

        Just d ->
            member (utctDay d) interval || (utctDay d > sup interval)

filterByBillable :: forall m. (MonadMemoize m, MonadPlanMillQuery m) => PM.Project -> m Bool
filterByBillable project = do
    kind <- projectKind project
    return $ kind == KindBillable

findTribeForProject :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m) => [Person] -> Project -> m (Tribe, Project)
findTribeForProject ppl project = do
    let uid = pProjectManager project
    let person = listToMaybe $ filter (\p -> pPlanmill p == uid) ppl
    let tribe = fromMaybe defaultTribe $ fmap pTribe person
    return (tribe, project)

findAssignmentsForProject :: forall m. (MonadPlanMillQuery m) => (Tribe, Project) -> m (Tribe, Text, PM.Assignments)
findAssignmentsForProject (tribe, project) = do
    let pid = _pId project
    assignments <- PMQ.assignments pid
    return (tribe, project ^. PM.pName, assignments)

findUserForAssignment :: forall m. (MonadPlanMillQuery m) => PM.Users -> (Tribe, Text, PM.Assignments) -> m (Tribe, Text, [PM.User])
findUserForAssignment planMillUsers (tribe, project, assignments) = do
    let assignedUsers = V.map PM.aPersonOrTeam assignments
    let pmuserMap = Map.fromList . map (\pu -> (PM._uId pu, pu)) $ V.toList planMillUsers
    let assignedToPMUsers = V.map (getUser pmuserMap) assignedUsers
    let cleaned = catMaybes $ V.toList assignedToPMUsers
    return (tribe, project, cleaned)
    where
        getUser :: Map PM.UserId PM.User -> Either PM.UserId PM.TeamId -> Maybe PM.User
        getUser pmus (Left uid) =
            Map.lookup uid pmus
        getUser _ _ =
            Nothing

planmillUserToPersonioEmployee
    :: forall m. (MonadPlanMillQuery m, MonadPersonio m)
        => HashMap FUM.Login (P.Employee, PM.User)
        -> (Tribe, Text, [PM.User])
        -> m (Tribe, Text, [P.Employee])
planmillUserToPersonioEmployee ppMap (tribe, project, users) = do
    let employees = map (pToP ppMap) users
    return (tribe, project, catMaybes employees)
    where
        pToP ppMap' user =
            fmap fst $ (=<<) (`HM.lookup` ppMap') $ PM.userLogin user

competencyData :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m) => m CompetencyReport
competencyData = do
    today <- currentDay
    let interval = beginningOfCurrMonth today ... pred today

    -- get projects
    projects <- PMQ.projects
    planMillUsers <- PMQ.users
    ppl <- peopleData

    -- filter projects by time interval (finish date during or after interval)
    let projectsForInterval = V.filter (ongoingProject interval) projects
    billableProjects <- V.filterM filterByBillable projectsForInterval

    -- map tribe to project by looking up team of project manager
    withTribe <- mapM (findTribeForProject ppl) billableProjects

    -- get assignments for each project
    withAssignments <- mapM findAssignmentsForProject withTribe

    -- map assigned users to Planmill user
    toUsers <- mapM (findUserForAssignment planMillUsers) withAssignments

    -- map Planmill user to personio user
    ppMap <- personioPlanmillMap
    toPersonio <- mapM (planmillUserToPersonioEmployee ppMap) toUsers

    -- get competence by _employeeRole
    -- flatmap project users to competencies and count uniques
    let withCompetences = V.map (\(t, p, u) -> (t, p, S.size . S.fromList $ map P._employeeRole u)) toPersonio

    -- group by tribe
    let grouped = L.groupBy (\(t1, _, _) (t2, _, _) -> t1 == t2) . L.sortBy (\(t1, _, _) (t2, _, _) -> compare t1 t2) $ V.toList withCompetences

    -- Return as type
    let rawReport = map (\((t, p, u):xs) -> (t, (ProjectCompetences p u : map (\(_, p', u') -> ProjectCompetences p' u') xs))) grouped
    let report = Map.fromList rawReport

    return $ CR report


-- HTML table

instance ToHtml CompetencyReport where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderCompetencyReport

renderCompetencyReport :: CompetencyReport -> HtmlPage "multdisciplinary-projects-count"
renderCompetencyReport (CR report) = page_ "Futurice’s ongoing multidisciplinary projects" $ do
    h1_ "Futurice’s ongoing multidisciplinary projects"

    table_ $ do
        thead_ $ do
            th_ ">3 competences"
            th_ ">4 competences"

        tbody_ $ do
            tr_ $ do
                td_ $ toHtml $ show . length . filter (>2) . map pcNumberOfCompetences . concat $ Map.elems report
                td_ $ toHtml $ show . length . filter (>3) . map pcNumberOfCompetences . concat $ Map.elems report

    table_ $ do
        thead_ $ do
            th_ "Tribe"
            th_ "Project"
            th_ "# of competences"

        tbody_ $ for_ (Map.assocs report) $ \(tribe, competences) -> tr_ $ do
            tr_ $ do
                td_ [rowspan_ $ T.pack $ show $ (+1) $ length competences] $ toHtml tribe
            for_ competences $ \comp -> do
                tr_ $ do
                    td_ $ toHtml $ pcProjectName comp
                    td_ $ toHtml $ show $ pcNumberOfCompetences comp
