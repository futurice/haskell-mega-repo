{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.Balances (
    -- * Report
    BalanceReport,
    balanceReport,
    -- * Types
    Balance (..),
    BalanceKind (..),
    -- * Logic
    balanceKind,
    balanceForUser,
    -- * Lenses
    balanceHours, balanceMissingHours,
    ) where

import Control.Lens              (sumOf)
import Data.Fixed                (Centi)
import Data.Ord                  (comparing)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Time
import Prelude ()

import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified FUM
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

import Futurice.App.Reports.MissingHours
       (missingHourCapacity, missingHoursForUser)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data BalanceKind = BalanceUnder | BalanceNormal | BalanceOver
  deriving (Eq, Ord)

makePrisms ''BalanceKind

instance ToJSON BalanceKind where
    toJSON BalanceUnder  = "under"
    toJSON BalanceNormal = "ok"
    toJSON BalanceOver   = "over"

instance ToHtml BalanceKind where
    toHtmlRaw = toHtml
    toHtml BalanceUnder  = b_ "under"
    toHtml BalanceNormal = "ok"
    toHtml BalanceOver   = b_ "over"

instance Csv.ToField BalanceKind where
    toField BalanceUnder  = "under"
    toField BalanceNormal = "ok"
    toField BalanceOver   = "over"

instance ReportValue BalanceKind

balanceKind :: (Num a, Ord a) => NDT 'Hours a -> BalanceKind
balanceKind h
    | h < (-20) = BalanceUnder
    | h > 40    = BalanceOver
    | otherwise = BalanceNormal
{-# INLINE balanceKind #-}

data Balance = Balance
    { _balanceHours        :: !(NDT 'Hours Centi)
    , _balanceMissingHours :: !(NDT 'Hours Centi)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

instance ToColumns Balance where
    type Columns Balance =
        '[ NDT 'Hours Centi , NDT 'Hours Centi, NDT 'Hours Centi, BalanceKind ]

    columnNames _ = K "flex" :* K "missing" :* K "balance" :* K "kind" :* Nil
    toColumns (Balance f m) =
        [ I f :* I m :* I diff :* I (balanceKind diff) :* Nil ]
      where
        diff = f + m -- if you mark missing, your flex will increase!

makeLenses ''Balance
deriveGeneric ''Balance
deriveVia [t| ToJSON Balance   `Via` Sopica Balance |]
deriveVia [t| FromJSON Balance `Via` Sopica Balance |]
instance ToSchema Balance where declareNamedSchema = sopDeclareNamedSchema


newtype Supervisor = Supervisor Text
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)

deriveGeneric ''Supervisor

instance ToColumns Supervisor where
    columnNames _ = K "supervisor" :* Nil

instance ToSchema Supervisor where declareNamedSchema = newtypeDeclareNamedSchema

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type BalanceReport = Report
    "Hour marking flex saldos"
    ReportGenerated
    (Vector :$ StrictPair Employee :$ StrictPair Supervisor :$ Balance)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

balanceForUser
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> PM.User
    -> m Balance
balanceForUser interval user = do
    let uid = user ^. PM.identifier
    PM.TimeBalance balanceMinutes <- PMQ.userTimebalance uid
    let balanceMinutes' = ndtConvert' balanceMinutes
    mh <- missingHoursForUser interval user
    pure $ Balance
        { _balanceHours        = balanceMinutes'
        , _balanceMissingHours = sumOf (folded . missingHourCapacity) mh
        }

balanceReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m, MonadPersonio m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => PM.Interval Day
    -> m BalanceReport
balanceReport interval = do
    now <- currentTime
    fumUsers <- fumEmployeeList
    fpm <- snd <$$> fumPlanmillMap
    fpm' <- itraverse (perUser $ supervisors fumUsers) fpm
    let fpm'' = V.fromList . sortBy cmpPE . HM.elems $ fpm'
    pure $ Report (ReportGenerated now) fpm''
  where
    cmpPE :: StrictPair Employee a -> StrictPair Employee a -> Ordering
    cmpPE = (comparing employeeTribe <> comparing employeeName) `on` S.fst

    supervisors :: Vector FUM.User -> HashMap FUM.Login Text
    supervisors us = HM.fromList . mapMaybe mk $ us'
      where
        us' = toList us

        mk u = (,) (u ^. FUM.userName) <$>
            join (flip HM.lookup ss <$> u ^. FUM.userSupervisor . lazy)

        -- id to name map
        ss :: HashMap Int Text
        ss = HM.fromList
            . map (\u -> (u ^. FUM.userId, u ^. FUM.userFullName))
            $ us'

    -- TODO: put planmillEmployee into fumPlanmillMap!
    -- Also MissingHours report
    perUser
        :: HashMap FUM.Login Text
        -> FUM.Login
        -> PM.User
        -> m (StrictPair Employee (StrictPair Supervisor Balance))
    perUser svs fumLogin pmUser = mk
        <$> planmillEmployee pmUid
        <*> pure (Supervisor $ fromMaybe "<unknown>" $ HM.lookup fumLogin svs)
        <*> balanceForUser interval pmUser
      where
        pmUid = pmUser ^. PM.identifier
        mk e s b = e S.:!: (s S.:!: b)
