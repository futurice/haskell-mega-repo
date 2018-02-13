{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
-- | Github users and their last activity approximation
module Futurice.App.Reports.GithubUsers (
    -- * Report
    GithubUsersReport,
    githubUsersReport,
    -- * Types
    UserInfo (..),
    -- * Lenses
    userInfoMember,
    userInfoLastActivity,
    userInfoHasActivity,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Trans.State.Strict (execState)
import Data.Foldable                    (foldl')
import Data.Maybe                       (isJust)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns

import qualified Data.Map.Strict as Map
import qualified GitHub          as GH

-- TODO: repositoryEventsR
import Unsafe.Coerce (unsafeCoerce)

-- TODO: add foldl' to futurice-prelude

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data UserInfo = UserInfo
    { _userInfoMember       :: !Bool  -- ^ if false: outside collaborator
    , _userInfoLastActivity :: !(Maybe UTCTime)
    , _userInfoHasActivity  :: !Bool
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''UserInfo
deriveGeneric ''UserInfo
instance ToColumns UserInfo
instance NFData UserInfo
deriveVia [t| ToJSON UserInfo `Via` Sopica UserInfo |]
instance ToSchema UserInfo where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

-- | TODO: use 'Futurice.App.Reports.GitHubUser' from "Futurice.App.Reports.FumGithub "
type GithubUsersReport = Report
    "GitHub users activity"
    ReportGenerated
    (Map (GH.Name GH.User) UserInfo)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- TODO: use futurice-integrations
githubUsersReport
    :: forall m env.
       ( MonadTime m, MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m GithubUsersReport
githubUsersReport = do
    now <- currentTime
    -- members
    orgName <- view githubOrganisationName

    ghUsers <- githubReq $ GH.membersOfR orgName GH.FetchAll
    let ghUids = GH.simpleUserLogin <$> ghUsers
    let m = Map.fromList $ map (\x -> (x, emptyInfo True)) $ toList ghUids

    -- repos
    repos <- fmap (sortOn GH.repoName . toList) $ githubReq $ GH.organizationReposR orgName GH.RepoPublicityAll GH.FetchAll

    -- collaborators
    coll <- fmap (concatMap toList) $ for (take 50 repos) $ \r ->
        githubReq $ GH.collaboratorsOnR
                (GH.simpleOwnerLogin $ GH.repoOwner r)
                (GH.repoName r) GH.FetchAll

    let collM = Map.fromList $ map (\x -> (GH.simpleUserLogin x, emptyInfo False)) coll

    -- Fetch events per each repo
    repoEvents <- traverse fetchEvents repos

    -- fetch activity
    -- TODO: change to applicative style
    let m' = fmap updateHasActivity $ foldl' applyActivity (Map.union m collM) repoEvents

    -- report
    pure $ Report (ReportGenerated now) m'
  where
    fetchEvents :: GH.Repo -> m (Vector GH.Event)
    fetchEvents r = githubReq $ (unsafeCoerce GH.repositoryEventsR
        (GH.simpleOwnerLogin $ GH.repoOwner r)
        (GH.repoName r)
        (GH.FetchAtLeast 199) :: GH.Request 'GH.RA (Vector GH.Event))

    applyActivity
        :: Map (GH.Name GH.User) UserInfo
        -> Vector GH.Event
        -> Map (GH.Name GH.User) UserInfo
    applyActivity m events = flip execState m $ for_ events $ \ev -> do
        let login = GH.simpleUserLogin $ GH.eventActor ev
        ix login . userInfoLastActivity %= Just . pickLatest (GH.eventCreatedAt ev)

    updateHasActivity a = a
        { _userInfoHasActivity = isJust (_userInfoLastActivity a)
        }

    pickLatest :: UTCTime -> Maybe UTCTime -> UTCTime
    pickLatest x Nothing  = x
    pickLatest x (Just y) = max x y

emptyInfo :: Bool -> UserInfo
emptyInfo member = UserInfo member Nothing False
