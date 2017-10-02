{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
-- | Common integrations requests.
module Futurice.Integrations.Common (
    -- * Date
    beginningOfCurrMonth,
    beginningOfPrevMonth,
    beginningOfPrev2Month,
    -- * FUM
    fumEmployeeList,
    flowdockOrganisation,
    githubOrganisationMembers,
    -- * PlanMill
    fumPlanmillMap,
    planmillEmployee,
    -- * Classy lenses
    HasFUMEmployeeListName(..),
    HasFlowdockOrgName(..),
    HasGithubOrgName(..),
    ) where

import Data.List                     (find)
import Data.Time
       (addGregorianMonthsClip, fromGregorian, toGregorian)
import Futurice.IdMap                (IdMap, idMapOf)
import Futurice.Integrations.Classes
import Futurice.Integrations.Types
import Futurice.Prelude
import Futurice.Tribe                (defaultTribe)
import Prelude ()
import Text.Regex.Applicative.Text   (match)

import qualified Data.HashMap.Strict as HM

import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH
import qualified Personio           as P
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

class HasFUMEmployeeListName a where
    fumEmployeeListName :: Lens' a FUM.ListName

class HasFlowdockOrgName a where
    flowdockOrganisationName :: Lens' a (FD.ParamName FD.Organisation)

class HasGithubOrgName a where
    githubOrganisationName :: Lens' a (GH.Name GH.Organization)

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
-- >>> beginningOfPrev2Month $(mkDay "2016-11-12")
-- 2016-09-01
beginningOfPrev2Month :: Day -> Day
beginningOfPrev2Month = addGregorianMonthsClip (-2) . beginningOfCurrMonth

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

-- | Get a mapping fum username to planmill user
--
-- Silently drops FUM users, which we cannot find planmill user for.
fumPlanmillMap
    :: ( MonadFUM m, MonadPlanMillQuery m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (HashMap FUM.Login (FUM.User, PM.User))
fumPlanmillMap =
    combine <$> fumEmployeeList <*> users
  where
    users = do
        us <- PMQ.users
        traverse (\u -> (u,) <$> PMQ.user (view PM.identifier u)) us

    combine :: Vector FUM.User -> Vector (PM.User, PM.User) -> HashMap FUM.Login (FUM.User, PM.User)
    combine fum pm = HM.fromList $ catMaybes $ map extract $ toList pm
      where
        fumNames :: IdMap FUM.User
        fumNames = idMapOf folded fum

        extract :: (PM.User, PM.User) -> Maybe (FUM.Login, (FUM.User, PM.User))
        extract (pmUser', pmUser) = do
            name <- match loginRe (PM.uUserName pmUser)
            fumUser <- fumNames ^. at name
            pure (name, (fumUser, update pmUser' pmUser))

        loginRe = "https://login.futurice.com/openid/" *> FUM.loginRegexp

    -- workaround for https://github.com/planmill/api/issues/11
    -- some data is present in users output but not in per-user
    update :: PM.User -> PM.User -> PM.User
    update u' u = u
        { PM.uCompetence = PM.uCompetence u <|> PM.uCompetence u'
        }

-- | Get information about employee from planmill
--
-- /TODO/: use applicative
planmillEmployee
    :: (MonadPlanMillQuery m, MonadPersonio m)
    => PM.UserId
    -> m Employee
planmillEmployee uid = do
    u <- PMQ.user uid

    -- tribe
    t <- case match loginRe (PM.uUserName u) of
        Nothing -> pure defaultTribe
        Just l  -> do
            ps <- P.personio P.PersonioEmployees
            let f p = p ^. P.employeeLogin == Just l
            pure $ fromMaybe defaultTribe $  view P.employeeTribe <$> find f ps

    -- contract
    c <- PMQ.enumerationValue (PM.uContractType u) "Unknown Contract"

    return $ Employee
        { employeeName     = PM.uFirstName u <> " " <> PM.uLastName u
        , employeeTribe    = t
        , employeeContract = c
        }
 where
   loginRe = "https://login.futurice.com/openid/" *> FUM.loginRegexp
