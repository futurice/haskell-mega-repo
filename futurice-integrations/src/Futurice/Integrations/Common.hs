{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- | Common integrations requests.
module Futurice.Integrations.Common (
    -- * Date
    beginningOfCurrMonth,
    beginningOfPrevMonth,
    endOfPrevMonth,
    beginningOfPrev2Month,
    previousFriday,
    -- * FUM
    fumEmployeeList,
    flowdockOrganisation,
    githubOrganisationMembers,
    -- * PlanMill
    personioPlanmillMap,
    personioPlanmillMap',
    planmillEmployee,
    planmillEmployeeToPersonio,
    -- * Okta
    githubUsernamesFromOkta,
    -- * Classy lenses
    HasFUMEmployeeListName(..),
    HasFlowdockOrgName(..),
    HasGithubOrgName(..),
    HasOktaGithubId(..),
    ) where

import Data.List                     (find)
import Data.Time
       (addDays, addGregorianMonthsClip, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate   (toWeekDate)
import Futurice.Integrations.Classes
import Futurice.Integrations.Types
import Futurice.Prelude
import Futurice.Tribe                (defaultTribe)
import Prelude ()

import qualified Data.HashMap.Strict as HM

import qualified Chat.Flowdock.REST as FD
import qualified Data.Map           as Map
import qualified FUM
import qualified GitHub             as GH
import qualified Okta               as O
import qualified Personio           as P
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

class HasFUMEmployeeListName a where
    fumEmployeeListName :: Lens' a FUM.ListName

class HasFlowdockOrgName a where
    flowdockOrganisationName :: Lens' a (FD.ParamName FD.Organisation)

class HasGithubOrgName a where
    githubOrganisationName :: Lens' a (GH.Name GH.Organization)

class HasOktaGithubId a where
    oktaGithubId :: a -> Text

-- |
--
-- >>> beginningOfCurrMonth $(mkDay "2016-11-12")
-- 2016-11-01
beginningOfCurrMonth :: Day -> Day
beginningOfCurrMonth = fromGregorian' . f. toGregorian
  where
    f (y, m, _) = (y, m, 1)

    fromGregorian' :: (Integer, Int, Int) -> Day
    fromGregorian' (y, m, d) = fromGregorian y m d

-- |
--
-- >>> beginningOfPrevMonth $(mkDay "2016-11-12")
-- 2016-10-01
beginningOfPrevMonth :: Day -> Day
beginningOfPrevMonth = addGregorianMonthsClip (-1) . beginningOfCurrMonth

-- |
--
-- >>> endOfPrevMonth $(mkDay "2016-11-12")
-- 2016-10-31
endOfPrevMonth :: Day -> Day
endOfPrevMonth = pred . beginningOfCurrMonth

-- |
--
-- >>> beginningOfPrev2Month $(mkDay "2016-11-12")
-- 2016-09-01
beginningOfPrev2Month :: Day -> Day
beginningOfPrev2Month = addGregorianMonthsClip (-2) . beginningOfCurrMonth

-- |
--
-- @2018-08-10@ is Friday:
--
-- >>> map previousFriday [ $(mkDay "2018-08-09") .. $(mkDay "2018-08-12") ]
-- [2018-08-03,2018-08-03,2018-08-10,2018-08-10]
--
-- >>> previousFriday $(mkDay "2018-08-10")
-- 2018-08-03
--
-- >>> previousFriday $(mkDay "2018-08-11")
-- 2018-08-10
--
previousFriday :: Day -> Day
previousFriday d
    | wd >= 6   = addDays (fromIntegral $ 5 - wd) d
    | otherwise = addDays (fromIntegral $ -2 - wd) d
  where
    (_, _, wd) = toWeekDate d

-- | Get list of active employees from FUM.
fumEmployeeList
    :: ( MonadFUM m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (Vector FUM.User)
fumEmployeeList = do
    listName <- view fumEmployeeListName
    FUM.fumList listName

-- | Get organisation from Flowdock
flowdockOrganisation
    :: (MonadFlowdock m, MonadReader env m, HasFlowdockOrgName env)
    => m FD.Organisation
flowdockOrganisation = do
    orgName <- view flowdockOrganisationName
    flowdockOrganisationReq orgName

-- | Get all members of the organisation.
githubOrganisationMembers
    :: ( MonadGitHub m
       , MonadGitHubC m (Vector GH.SimpleUser)
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.SimpleUser)
githubOrganisationMembers = do
    orgName <- view githubOrganisationName
    githubReq $ GH.membersOfR orgName GH.FetchAll

githubUsernamesFromOkta :: (MonadOkta m, HasOktaGithubId ctx) => ctx -> m (Map.Map Text (Maybe (GH.Name GH.User))) -- TODO: Change to have Email datatype
githubUsernamesFromOkta ctx = do
    let appId = oktaGithubId ctx
    oktaUsers <- O.users
    oktaAppUsers <- O.appUsers appId
    let appUsersMap = Map.fromList $ map (\u -> (O.appUserId u, O.credUserName $ O.appUserCredentials u)) oktaAppUsers
    let userMap = Map.fromList $ map (\u -> (O.profileLogin $ O.userProfile u, GH.mkUserName <$> Map.lookup (O.userId u) appUsersMap )) oktaUsers
    pure userMap

personioPlanmillMap
    :: (MonadPersonio m, MonadPlanMillQuery m)
    => m (HashMap FUM.Login (P.Employee, PM.User))
personioPlanmillMap = personioPlanmillMap' <*> P.personio P.PersonioEmployees

personioPlanmillMap'
    :: MonadPlanMillQuery m
    => m ([P.Employee] -> HashMap FUM.Login (P.Employee, PM.User))
personioPlanmillMap' = flip combine <$> users
  where
    -- detailed planmill users
    users = do
        us <- PMQ.users
        traverse (\u -> updatePmUser u <$> PMQ.user (view PM.identifier u)) (toList us)

    -- workaround for https://github.com/planmill/api/issues/11
    -- some data is present in users output but not in per-user
    updatePmUser :: PM.User -> PM.User -> PM.User
    updatePmUser u' u = u
        { PM.uCompetence = PM.uCompetence u <|> PM.uCompetence u'
        }

    combine :: [P.Employee] -> [PM.User] -> HashMap FUM.Login (P.Employee, PM.User)
    combine ps pms = HM.intersectionWith (,) ps' pms'
      where
        ps' = HM.fromList $ flip mapMaybe ps $ \e -> (,e) <$> e ^. P.employeeLogin
        pms' = HM.fromList $ flip mapMaybe pms $ \u -> (,u) <$> PM.userLogin u

-- | Get information about employee from planmill
planmillEmployee
    :: (MonadPlanMillQuery m, MonadPersonio m, MonadMemoize m)
    => PM.UserId
    -> m Employee
planmillEmployee uid = do
    u <- PMQ.user uid

    -- tribe
    t <- case PM.userLogin u of
        Nothing -> pure defaultTribe
        Just l  -> do
            ps <- P.personio P.PersonioEmployees
            let f p = p ^. P.employeeLogin == Just l
            pure $ fromMaybe defaultTribe $  view P.employeeTribe <$> find f ps

    -- contract
    c <- PMQ.enumerationValue (PM.uContractType u) "Unknown Contract"

    return Employee
        { employeeName     = PM.uFirstName u <> " " <> PM.uLastName u
        , employeeTribe    = t
        , employeeContract = c
        }

planmillEmployeeToPersonio :: (MonadPlanMillQuery m, MonadPersonio m, MonadMemoize m) => PM.UserId -> m (Maybe P.Employee)
planmillEmployeeToPersonio uid = do
    u <- PMQ.user uid
    ps <- P.personio P.PersonioEmployees
    let f p = p ^. P.employeeLogin == PM.userLogin u
    pure $ find f ps

-- $setup
-- >>> :set -XTemplateHaskell
