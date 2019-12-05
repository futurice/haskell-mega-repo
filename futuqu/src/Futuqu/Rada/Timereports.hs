{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Rada.Timereports where

import Control.Lens                         (toListOf)
import Data.Fixed                           (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Integrations.TimereportKind
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Prelude ()

import qualified Data.Text        as T
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- There aren't timereport id. not sure it's needed.
data Timereport = Timereport
    -- identifiers
    { trUserId    :: PM.UserId
    , trTaskId    :: !(Maybe PM.TaskId)     -- ^ task etc. ids are Nothing for absences
    , trProjectId :: !(Maybe PM.ProjectId)
    , trAccountId :: !(Maybe PM.AccountId)

    -- planmill
    , trDay            :: !Day
    , trHours          :: !(NDT 'Hours Centi)
    , trComment        :: !Text
    , trStatus         :: !Text -- TODO: uset EnumTextValue
    , trBillableStatus :: !Text --
    -- Note: dutyType is left out

    -- business knowledge
    , trKind   :: !TimereportKind
    }
    deriving (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Timereport         `Via` Sopica Timereport |]
deriveVia [t| FromJSON Timereport       `Via` Sopica Timereport |]
deriveVia [t| DefaultOrdered Timereport `Via` Sopica Timereport |]
deriveVia [t| ToRecord Timereport       `Via` Sopica Timereport |]
deriveVia [t| ToNamedRecord Timereport  `Via` Sopica Timereport |]

instance ToSchema Timereport where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

timereportsData
    :: (MonadPlanMillQuery m, MonadPersonio m, MonadMemoize m)
    => Month
    -> m [Timereport]
timereportsData month = do
    let interval = monthInterval month

    users <- PMQ.users
    sts <- fmap2 T.toLower $ PMQ.allEnumerationValues Proxy Proxy
    bss <- fmap2 T.toLower $ PMQ.allEnumerationValues Proxy Proxy

    timereportsData' interval users sts bss
  where
    fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
    fmap2 = fmap . fmap

timereportsData'
    :: (MonadPlanMillQuery m, MonadPersonio m, MonadMemoize m, Traversable f)
    => PM.Interval Day
    -> f PM.User
    -> Map (PM.EnumValue PM.Timereport "status") Text
    -> Map (PM.EnumValue PM.Timereport "billableStatus") Text
    -> m [Timereport]
timereportsData' interval users sts bss =
    fmap (toListOf $ folded . folded) $ for (toList users) $ \u -> do
        let uid = u ^. PM.identifier

        trs <- PMQ.timereports interval uid

        for (toList trs) $ \tr -> do
            ki <- timereportKindFutuqu tr

            prjId <- case PM.trProject tr of
                Just pid -> return (Just pid)
                Nothing  -> PM.taskProject <$> PMQ.task (PM.trTask tr)

            mproject <- traverse PMQ.simpleProject prjId
            let accId = mproject >>= (^. PM.pAccount)

            return Timereport
                { trUserId = uid

                , trTaskId    = unlessAbsence  ki $ PM.trTask tr
                , trProjectId = unlessAbsence' ki prjId
                , trAccountId = unlessAbsence' ki accId

                , trDay = PM.trStart tr
                , trHours = ndtConvert' $ PM.trAmount tr
                , trComment = fromMaybe "-" $ PM.trComment tr

                , trStatus = fromMaybe "-" $ sts ^? ix (PM.trStatus tr)
                , trBillableStatus = fromMaybe "-" $ bss ^? ix (PM.trBillableStatus tr)

                , trKind = ki
                }
  where
    unlessAbsence ki _ | timereportKindIsAbsence ki = Nothing
    unlessAbsence _  x = Just x

    unlessAbsence' ki _ | timereportKindIsAbsence ki = Nothing
    unlessAbsence' _  x = x

