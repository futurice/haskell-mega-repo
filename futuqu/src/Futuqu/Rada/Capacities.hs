{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Rada.Capacities where

import Control.Lens          (toListOf)
import Data.Fixed            (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Prelude ()

import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Capacity = Capacity
    -- identifiers
    { capUserId :: PM.UserId
    -- planmill
    , capDay   :: !Day
    , capHours :: !(NDT 'Hours Centi)
    }
    deriving (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Capacity   `Via` Sopica Capacity |]
deriveVia [t| FromJSON Capacity `Via` Sopica Capacity |]

instance ToSchema Capacity where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

capacitiesData
    :: forall m. (MonadPlanMillQuery m, MonadPersonio m)
    => Month
    -> m [Capacity]
capacitiesData month = do
    let interval = monthInterval month

    ppm <- personioPlanmillMap
    toListOf (folded . folded) <$> traverse (fetch interval) ppm
  where
    fetch :: PM.Interval Day -> (P.Employee, PM.User) -> m [Capacity]
    fetch interval (pe, pm) = do
        let pmId = pm ^. PM.identifier
        caps <- PMQ.capacities interval pmId

        return $ flip mapMaybe (toList caps) $ \uc -> do
            let day = PM.userCapacityDate uc

            guard $ PM.userCapacityAmount uc > 0

            -- capacity is zero outside the contact span
            mcase (pe ^. P.employeeHireDate) mzero $ \hireDate -> do
                guard $ hireDate <= day
                guard $ maybe True (day <=) (pe ^. P.employeeEndDate)

                return Capacity
                    { capUserId = pmId
                    , capDay    = day
                    , capHours  = ndtConvert' (PM.userCapacityAmount uc)
                    }
