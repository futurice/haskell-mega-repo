{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Ggrr.MissingHours where

import Data.Fixed            (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time
import Prelude ()

import qualified Data.Map.Strict  as Map
import qualified PlanMill         as PM

import Futuqu.Rada.Capacities
import Futuqu.Rada.Timereports

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data MissingHour = MissingHour
    -- identifiers
    { mhUserId :: PM.UserId
    -- data
    , mhDay   :: !Day
    , mhHours :: !(NDT 'Hours Centi)
    }
    deriving (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON MissingHour         `Via` Sopica MissingHour |]
deriveVia [t| FromJSON MissingHour       `Via` Sopica MissingHour |]
deriveVia [t| DefaultOrdered MissingHour `Via` Sopica MissingHour |]
deriveVia [t| ToRecord MissingHour       `Via` Sopica MissingHour |]
deriveVia [t| ToNamedRecord MissingHour  `Via` Sopica MissingHour |]

instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

missingHoursData
    :: forall m. (MonadPlanMillQuery m, MonadPersonio m)
    => Month
    -> m [MissingHour]
missingHoursData month = do
    css <- capacitiesData month
    trs <- timereportsData month

    -- aggregate both capacities and timereports by user, day
    let css' :: Map (PM.UserId, Day) (NDT 'Hours Centi)
        css' = Map.fromListWith (+)
            [ ((capUserId c, capDay c), capHours c)
            | c <- css
            ]

    -- Note: we filter days with non-zero hours marked.
    -- You can mark zero hours.
    let trs' :: Map (PM.UserId, Day) (NDT 'Hours Centi)
        trs' = Map.filter (> 0) $ Map.fromListWith (+)
            [ ((trUserId tr, trDay tr), trHours tr)
            | tr <- trs
            -- note: we don't filter out any absences.
            -- even a balance absence counts as "day marked"
            ]

    let diff :: Map (PM.UserId, Day) (NDT 'Hours Centi)
        diff = Map.difference css' trs'

    return [ MissingHour uid d h | ((uid, d), h) <- Map.toList diff ]
