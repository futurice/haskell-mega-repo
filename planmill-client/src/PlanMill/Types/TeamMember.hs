{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module PlanMill.Types.TeamMember (
    TeamMember(..),
    TeamMembers,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.User (UserId)

type TeamMembers = Vector TeamMember

data TeamMember = TeamMember
    { tmId          :: UserId
    , tmPrimaryTeam :: Bool
    }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''TeamMember

instance Hashable TeamMember
instance NFData TeamMember
instance AnsiPretty TeamMember
instance Binary TeamMember
instance HasStructuralInfo TeamMember where structuralInfo = sopStructuralInfo
instance HasSemanticVersion TeamMember

instance FromJSON TeamMember where
    parseJSON = withObject "TeamMember" $ \obj -> TeamMember
        <$> obj .: "id" -- sometimes team member isn't a person?
        <*> primaryTeam obj
      where
        primaryTeam obj = do
            i <- obj .: "primaryTeam"
            case i :: Int of
                0 -> return False
                1 -> return True
                _ -> fail $ "Unknown primaryTeam " ++ show i
