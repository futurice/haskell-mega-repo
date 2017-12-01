{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Planmill Employees
module Futurice.App.Reports.PlanmillEmployees (
    -- * Report
    PlanmillEmployeesReport,
    planmillEmployeesReport,
    -- * Types
    PlanmillEmployee (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns

import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PlanmillEmployee = PlanmillEmployee
    { _pmeFirstName    :: !Text
    , _pmeLastName     :: !Text
    , _pmeTitle        :: !Text
    , _pmeSuperior     :: !Text
    , _pmeTeam         :: !Text
    , _pmeContract     :: !Text
    , _pmePhone        :: !Text
    , _pmeEmail        :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- makeLenses ''PlanmillEmployee
deriveGeneric ''PlanmillEmployee

instance NFData PlanmillEmployee
instance ToJSON PlanmillEmployee where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON PlanmillEmployee where parseJSON = sopParseJSON
instance ToSchema PlanmillEmployee where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns PlanmillEmployee where
    columnNames _ =
        K "first-name" :*
        K "last-name" :*
        K "title" :*
        K "superior" :*
        K "team" :*
        K "contract" :*
        K "phone" :*
        K "email" :*
        Nil

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type PlanmillEmployeesReport = Report
    "Employees in PlanMill"
    ReportGenerated
    (Vector PlanmillEmployee) 

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

planmillEmployeesReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m PlanmillEmployeesReport
planmillEmployeesReport = do
    now <- currentTime
    users <- PMQ.users
    users' <- traverse mk users
    pure $ Report
        (ReportGenerated now)
        (introsortBy (compare `on` sortKey) users')
  where
    mk :: PM.User -> m PlanmillEmployee
    mk u = do
        contract <- PMQ.enumerationValue (PM.uContractType u) "Unknown contract"
        pure $ PlanmillEmployee
            { _pmeFirstName = PM.uFirstName u
            , _pmeLastName  = PM.uLastName u
            , _pmeTitle     = "TODO: get from FUM"
            , _pmeSuperior  = "TODO: get from FUM"
            , _pmeTeam      = "?" -- fromMaybe "?" (PM.uTeamName u) -- TODO
            , _pmeContract  = contract
            , _pmePhone     = "TODO: get from FUM"
            , _pmeEmail     = "TODO: " <> fromMaybe "" (PM.uEmail u)
            }

    sortKey = (,) <$> _pmeLastName <*> _pmeFirstName
