{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module PlanMill.Types.SimpleProject where

import PlanMill.Internal.Prelude

import PlanMill.Types.Account     (AccountId)
import PlanMill.Types.Enumeration (EnumValue (..))
import PlanMill.Types.Project

-- Data type that has all the field that are populated
-- by Planmill when fetching singular project
-- (so not those field that are got only by querying
-- all projects)
data SimpleProject = SimpleProject
    { _simpleProjectId            :: !ProjectId
    , _simpleProjectName          :: !Text
    , _simpleProjectAccount       :: !(Maybe AccountId)
    , _simpleProjectCategory      :: !(EnumValue Project "category")
    , _simpleProjectOperationalId :: !(Maybe Int)
    , _simpleProjectPortfolioId   :: !(Maybe PortfolioId)
    } deriving (Generic)

makeLenses ''SimpleProject
deriveGeneric ''SimpleProject

class HasSimpleProject p where
    sProject :: Lens' p SimpleProject

    pId :: Lens' p ProjectId
    pId = sProject . simpleProjectId

    pName :: Lens' p Text
    pName = sProject . simpleProjectName

    pAccount :: Lens' p (Maybe AccountId)
    pAccount = sProject . simpleProjectAccount

    pCategory :: Lens' p (EnumValue Project "category")
    pCategory = sProject . simpleProjectCategory

    pOperationalId :: Lens' p (Maybe Int)
    pOperationalId = sProject . simpleProjectOperationalId

instance HasSimpleProject SimpleProject where
    sProject = id

instance HasSimpleProject Project where
    sProject = lens f g where
      f Project {..} = SimpleProject
          { _simpleProjectId            = _pId
          , _simpleProjectName          = _pName
          , _simpleProjectAccount       = _pAccount
          , _simpleProjectCategory      = _pCategory
          , _simpleProjectOperationalId = _pOperationalId
          , _simpleProjectPortfolioId   = _pPortfolioId
          }

      g p SimpleProject {..} = p
          { _pId              = _simpleProjectId
          , _pName          = _simpleProjectName
          , _pAccount       = _simpleProjectAccount
          , _pCategory      = _simpleProjectCategory
          , _pOperationalId = _simpleProjectOperationalId
          , _pPortfolioId      = _simpleProjectPortfolioId
          }
