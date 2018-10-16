{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Futurice.Integrations.TimereportKind where

import Futurice.Prelude
import Prelude ()
import Futurice.Integrations

import qualified PlanMill as PM
import qualified PlanMill.Queries as PMQ

data TimereportKind
    = KindBillable        -- ^ billable work
    | KindNonBillable     -- ^ non-billable client work
    | KindInternal        -- ^ non-billable internal work: @account type: "My Company"@
    | KindAbsence         -- ^ absences: @project category = "Absence"@
    | KindSickLeave       -- ^ sick leave
    | KindBalanceAbsence  -- ^ balance absence
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

timereportKind :: MonadPlanMillQuery m => PM.Timereport -> m TimereportKind
timereportKind tr = do
    billableStatus <- PMQ.enumerationValue (PM.trBillableStatus tr) "Non-billable"
    dutyType <- traverse (`PMQ.enumerationValue` "D") (PM.trDutyType tr)

    pid <- case PM.trProject tr of
        Just pid -> return (Just pid)
        Nothing  -> PM.taskProject <$> PMQ.task (PM.trTask tr)

    mproject <- traverse PMQ.project pid
    maccount <- traverse PMQ.account (mproject >>= PM.pAccount)

    mprojectCategory <- traverse (`PMQ.enumerationValue` "C?") (PM.pCategory <$> mproject)
    maccountType     <- traverse (`PMQ.enumerationValue` "T?") (PM.saType <$> maccount)

    return $ kind mprojectCategory maccountType billableStatus dutyType
  where
    kind 
        :: Maybe Text  -- ^ project type
        -> Maybe Text  -- ^ account type
        -> Text        -- ^ billable status
        -> Maybe Text  -- ^ duty type
        -> TimereportKind
    kind _                _                   "Non-billable" (Just "Balance leave")                     = KindBalanceAbsence
    kind _                _                   "Non-billable" (Just "Sick leave")                        = KindSickLeave
    kind _                _                   "Non-billable" (Just "Sick leave by medical certificate") = KindSickLeave
    kind (Just "Absence") _                   "Non-billable" _                                          = KindAbsence
    kind _                (Just "My company") "Non-billable" _                                          = KindInternal
    kind _                _                   "Non-billable" _                                          = KindNonBillable
    kind _                _                   _              _                                          = KindBillable

-- | This is not precise: All absences are smashed.
projectKind :: MonadPlanMillQuery m => PM.Project -> m TimereportKind
projectKind p = do
    maccount <- traverse PMQ.account (PM.pAccount p)

    projectCategory  <- PMQ.enumerationValue (PM.pCategory p) "C?"
    maccountType     <- traverse (`PMQ.enumerationValue` "T?") (PM.saType <$> maccount)
  
    return $ kind projectCategory maccountType
  where
    kind "Absence" _                   = KindAbsence
    kind _         (Just "My company") = KindInternal
    kind _         _                   = KindBillable
