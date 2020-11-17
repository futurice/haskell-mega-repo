{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- TODO: split into submodules
module Power.Types where

import Data.Aeson            (withObject, (.:), (.:?))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import FUM.Types.Login       (Login)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Power.PyJSON

import qualified PlanMill as PM

newtype PersonId = PersonId Int deriving newtype (Eq, Ord, Show, NFData, FromJSON, ToJSON)
newtype ProjectId = ProjectId Int deriving newtype (Eq, Ord, Show, NFData, FromJSON, ToSchema, ToJSON)
newtype TribeId = TribeId Int deriving newtype (Eq, Ord, Show, NFData, FromJSON, ToSchema, ToJSON)
newtype CustomerId = CustomerId Int deriving newtype (Eq, Ord, Show, FromJSON)

-------------------------------------------------------------------------------
-- /people
-------------------------------------------------------------------------------

data Person = Person
    { personLogin     :: Login  -- ^ TODO: remove when Power will have Personio ID
    , personUtzTarget :: Int    -- ^ integer percentages
    , personId        :: PersonId
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica Person)

instance PyJSON Person where
    parsePyJSON = withObject "Person" $ \obj -> Person
        <$> obj .: "username"
        <*> obj .: "utz_target"
        <*> obj .: "id"

-------------------------------------------------------------------------------
-- /customer
-------------------------------------------------------------------------------

data Customer = Customer
    { customerId               :: !CustomerId
    , customerName             :: !Text
    , customerInternalCustomer :: !Bool
    } deriving Show

instance PyJSON Customer where
    parsePyJSON = withObject "Customer" $ \obj -> Customer
        <$> obj .: "id"
        <*> obj .: "name"
        <*> obj .: "internal_customer"

-------------------------------------------------------------------------------
-- /project
-------------------------------------------------------------------------------

data Project = Project
    { projectId         :: !ProjectId
    , projectName       :: !Text
    , projectCustomerId :: !CustomerId
    , projectTribeId    :: !TribeId
    } deriving (Eq, Ord, Show)

instance PyJSON Project where
    parsePyJSON = withObject "Project" $ \obj -> Project
        <$> obj .: "id"
        <*> obj .: "name"
        <*> obj .: "customer_id"
        <*> obj .: "tribe_id"

-------------------------------------------------------------------------------
-- /allocation
-------------------------------------------------------------------------------

data Allocation = Allocation
    { allocationProposed        :: !Bool
    , allocationPersonId        :: !(Maybe PersonId)
    , allocationProjectId       :: !ProjectId
    , allocationTotalAllocation :: !Double
    , allocationStartDate       :: !UTCTime
    , allocationEndDate         :: !UTCTime
    } deriving Show

instance PyJSON Allocation where
    parsePyJSON = withObject "Allocation" $ \obj -> Allocation
        <$> obj .: "proposed"
        <*> obj .:? "person_id"
        <*> obj .: "project_id"
        <*> obj .: "total_allocation"
        <*> (posixSecondsToUTCTime . (/ 1000) <$> obj .: "start_date")
        <*> (posixSecondsToUTCTime . (/ 1000) <$> obj .: "end_date")

-------------------------------------------------------------------------------
-- /powerprojecttoplanmillproject
-------------------------------------------------------------------------------

data ProjectMapping = ProjectMapping
    { _pmPowerProjectId    :: !ProjectId
    , _pmPlanMillProjectId :: !PM.ProjectId
    , _pmManuallySet       :: !Bool
    } deriving Show

instance PyJSON ProjectMapping where
    parsePyJSON = withObject "ProjectMapping" $ \obj -> ProjectMapping
        <$> obj .: "power_project_id"
        <*> obj .: "planmill_project_id"
        <*> obj .: "manually_set"

-------------------------------------------------------------------------------
-- /tribe
-------------------------------------------------------------------------------

data Tribe = Tribe
    { tribeId   :: !TribeId
    , tribeName :: !Text
    } deriving Show

instance PyJSON Tribe where
    parsePyJSON = withObject "Tribe" $ \obj -> Tribe
        <$> obj .: "id"
        <*> obj .: "name"
