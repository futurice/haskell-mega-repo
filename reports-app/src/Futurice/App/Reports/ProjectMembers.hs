{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.ProjectMembers where

import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data ProjectMember = ProjectMember
    { memberId   :: !PM.UserId
    , memberName :: !Text
    } deriving (GhcGeneric, SopGeneric,ToSchema, HasDatatypeInfo, NFData)
      deriving (FromJSON, ToJSON) via (Sopica ProjectMember)

data ProjectMembers = ProjectMembers
    { prjmProjectId      :: !PM.ProjectId
    , prjmName           :: !Text
    , prjmProjectMembers :: ![ProjectMember]
    } deriving (GhcGeneric, SopGeneric,ToSchema, HasDatatypeInfo, NFData)
      deriving (FromJSON, ToJSON) via (Sopica ProjectMembers)

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

projectMemberData :: (MonadPlanMillQuery m) => m [ProjectMembers]
projectMemberData = do
    prjs <- PMQ.projects
    traverse toProjectMembers $ toList prjs
  where
    toProjectMembers project = do
        members <- PMQ.projectMembers (PM._pId project)
        pure $ ProjectMembers
            { prjmProjectId  = PM._pId project
            , prjmName       = PM.pName project
            , prjmProjectMembers = fmap (\x -> ProjectMember (PM._projectMemberUserId x) (PM._projectMemberName x)) $ toList members
            }
