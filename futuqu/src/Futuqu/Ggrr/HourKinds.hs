{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Ggrr.HourKinds where

import Data.Fixed                           (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Integrations.TimereportKind
import Futurice.Prelude
import Futurice.Time
import Prelude ()
import Data.Set.Lens (setOf)

import qualified Data.Map.Strict as Map
import qualified PlanMill        as PM

import Futuqu.Rada.People
import Futuqu.Rada.Timereports

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data HourKind = HourKind
    -- identifiers
    { hkUserId :: PM.UserId
    -- data
    , hkBillableHours    :: !(NDT 'Hours Centi)
    , hkNonBillableHours :: !(NDT 'Hours Centi)
    , hkInternalHours    :: !(NDT 'Hours Centi)
    , hkAbsenceHours     :: !(NDT 'Hours Centi)
    -- additional data
    , hkUtz              :: !Double
    , hkUtzTarget        :: !(Maybe Double) -- we could default to 0. we don't.

    -- TODO: add value creation?
    }
    deriving (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON HourKind         `Via` Sopica HourKind |]
deriveVia [t| FromJSON HourKind       `Via` Sopica HourKind |]
deriveVia [t| DefaultOrdered HourKind `Via` Sopica HourKind |]
deriveVia [t| ToRecord HourKind       `Via` Sopica HourKind |]
deriveVia [t| ToNamedRecord HourKind  `Via` Sopica HourKind |]

instance ToSchema HourKind where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

hourKindsData
    :: forall m. (MonadTime m, MonadPlanMillQuery m, MonadPersonio m, MonadPower m)
    => Month
    -> m [HourKind]
hourKindsData month = do
    people <- peopleData

    let pUtz :: Map PM.UserId Int
        pUtz = Map.fromList
            [ (uid, utzTarget)
            | p <- people
            , uid <- toList (pPlanmill p)
            , utzTarget <- toList (pUtzTarget p)
            ]

    trs <- timereportsData month

    let uids = setOf (folded . getter trUserId) trs

    let trs' :: Map (PM.UserId, TimereportKind) (NDT 'Hours Centi)
        trs' = Map.filter (> 0) $ Map.fromListWith (+)
            [ ((trUserId tr, trKind tr), trHours tr)
            | tr <- trs
            ]

    return
        [ HourKind
            { hkUserId = uid

            , hkBillableHours    = billable
            , hkNonBillableHours = nonBillable
            , hkInternalHours    = internal
            , hkAbsenceHours     = absence

            , hkUtz       =
                  if total == 0
                  then 0
                  else 100 * realToFrac (unNDT billable) / realToFrac (unNDT total)
            , hkUtzTarget = pUtz ^? ix uid . getter fromIntegral

            }

        | uid <- toList uids

        , let billable    = trs' ^. ix (uid, KindBillable)
        , let nonBillable = trs' ^. ix (uid, KindNonBillable)
        , let internal    = trs' ^. ix (uid, KindInternal)
        , let absence     = trs' ^. ix (uid, KindAbsence)
                          + trs' ^. ix (uid, KindSickLeave)

        , let total = billable + nonBillable + internal -- note: doesn't include absences.
        ]
