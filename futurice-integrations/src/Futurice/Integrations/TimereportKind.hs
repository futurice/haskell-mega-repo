{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.Integrations.TimereportKind where

import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv         as Csv
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data TimereportKind
    = KindBillable        -- ^ billable work
    | KindNonBillable     -- ^ non-billable client work
    | KindInternal        -- ^ non-billable internal work: @account type: "My Company"@ or project type Team Internal work
    | KindAbsence         -- ^ absences: @project category = "Absence"@
    | KindSickLeave       -- ^ sick leave
    | KindBalanceAbsence  -- ^ balance absence
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, Lift)
  deriving anyclass (NFData)

timereportKindIsAbsence :: TimereportKind -> Bool
timereportKindIsAbsence KindAbsence        = True
timereportKindIsAbsence KindSickLeave      = True
timereportKindIsAbsence KindBalanceAbsence = True
timereportKindIsAbsence _ = False

-------------------------------------------------------------------------------
-- More instances
-------------------------------------------------------------------------------

makePrisms ''TimereportKind
deriveGeneric ''TimereportKind

instance TextEnum TimereportKind where
    type TextEnumNames TimereportKind =
        '["billable", "non-billable", "internal"
        , "absence", "sick-leave", "absence"
        ]

deriveVia [t| Arbitrary TimereportKind       `Via` Sopica TimereportKind  |]
deriveVia [t| ToJSON TimereportKind          `Via` Enumica TimereportKind |]
deriveVia [t| FromJSON TimereportKind        `Via` Enumica TimereportKind |]
deriveVia [t| ToHttpApiData TimereportKind   `Via` Enumica TimereportKind |]
deriveVia [t| FromHttpApiData TimereportKind `Via` Enumica TimereportKind |]
deriveVia [t| Csv.ToField TimereportKind     `Via` Enumica TimereportKind |]
deriveVia [t| Csv.FromField TimereportKind   `Via` Enumica TimereportKind |]
deriveVia [t| ToHtml TimereportKind          `Via` Enumica TimereportKind |]

instance ToParamSchema TimereportKind where toParamSchema = enumToParamSchema
instance ToSchema TimereportKind where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

-- | Sick leave data is sensitive so don't highlight unless really necessary
timereportKindFutuqu :: (MonadPlanMillQuery m, MonadMemoize m) => PM.Timereport -> m TimereportKind
timereportKindFutuqu tr = do
    t <- timereportKind tr
    case t of
      KindSickLeave -> pure KindAbsence
      _             -> pure t

timereportKind :: (MonadPlanMillQuery m, MonadMemoize m) => PM.Timereport -> m TimereportKind
timereportKind tr = do
    billableStatus <- PMQ.enumerationValue (PM.trBillableStatus tr) "Non-billable"
    dutyType <- traverse (`PMQ.enumerationValue` "D") (PM.trDutyType tr)

    pid <- case PM.trProject tr of
        Just pid -> return (Just pid)
        Nothing  -> PM.taskProject <$> PMQ.task (PM.trTask tr)

    mproject <- traverse PMQ.simpleProject pid
    maccount <- traverse PMQ.account (mproject >>= (^. PM.pAccount))

    mprojectCategory <- traverse (`PMQ.enumerationValue` "C?") ((^. PM.pCategory) <$> mproject)
    maccountType     <- traverse (`PMQ.enumerationValue` "T?") (PM.saType <$> maccount)

    return $ kind mprojectCategory maccountType billableStatus dutyType
  where
    kind
        :: Maybe Text  -- ^ project type
        -> Maybe Text  -- ^ account type
        -> Text        -- ^ billable status
        -> Maybe Text  -- ^ duty type
        -> TimereportKind
    kind _                           _                   "Non-billable" (Just "Balance leave")                     = KindBalanceAbsence
    kind _                           _                   "Non-billable" (Just "Sick leave")                        = KindSickLeave
    kind _                           _                   "Non-billable" (Just "Sick leave by medical certificate") = KindSickLeave
    kind (Just "Absence")            _                   "Non-billable" _                                          = KindAbsence
    kind _                           (Just "My company") "Non-billable" _                                          = KindInternal
    kind (Just "Team internal work") _                   "Non-billable" _                                          = KindInternal
    kind _                           _                   "Non-billable" _                                          = KindNonBillable
    kind _                           _                   _              _                                          = KindBillable

-- | This is not precise: All absences are smashed.
projectKind :: (MonadPlanMillQuery m, MonadMemoize m, PM.HasSimpleProject p) => p -> m TimereportKind
projectKind p = do
    maccount <- traverse PMQ.account (p ^. PM.pAccount)

    projectCategory  <- PMQ.enumerationValue (p ^. PM.pCategory) "C?"
    maccountType     <- traverse (`PMQ.enumerationValue` "T?") (PM.saType <$> maccount)

    return $ kind projectCategory maccountType
  where
    kind "Absence" _                   = KindAbsence
    kind _         (Just "My company") = KindInternal
    kind _         _                   = KindBillable
