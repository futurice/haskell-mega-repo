{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.TimereportsByTask (
    -- * Report
    TimereportsByTaskReport,
    timereportsByTaskReport,
    -- * Types
    TimereportsByTask (..),
    ) where

import Data.Fixed                (Centi)
import Data.Fold                 (L' (..), filtering, run)
import Futurice.Generics
import Futurice.Integrations
import Futurice.List
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Time
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.TDigest       as TDigest
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data TimereportsStats = TimereportsStats
    { _tsHours  :: !(NDT 'Hours Centi)
    , _tsCount  :: !Int
    , _tsMedian :: !(NDT 'Hours Centi)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''TimereportsStats
deriveVia [t| ToJSON TimereportsStats   `Via` Sopica TimereportsStats |]
deriveVia [t| FromJSON TimereportsStats `Via` Sopica TimereportsStats |]
instance ToColumns TimereportsStats
instance ToSchema TimereportsStats where declareNamedSchema = sopDeclareNamedSchema

data TimereportsByTask = TimereportsByTask
    { _tbtAccountName    :: !Text
    , _tbtProjectName    :: !Text
    , _tbtTaskName       :: !Text
    , _tbtHoursOnebMonth :: !TimereportsStats
    , _tbtHoursPrevMonth :: !TimereportsStats
    , _tbtHoursCurrMonth :: !TimereportsStats
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

-- makeLenses ''TimereportsByTask
deriveGeneric ''TimereportsByTask
deriveVia [t| ToJSON TimereportsByTask   `Via` Sopica TimereportsByTask |]
deriveVia [t| FromJSON TimereportsByTask `Via` Sopica TimereportsByTask |]
instance ToSchema TimereportsByTask where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns TimereportsByTask where
    type Columns TimereportsByTask =
        Append '[Text, Text, Text]
        (Append (Columns TimereportsStats)
        (Append (Columns TimereportsStats)
        (Columns TimereportsStats)))

    columnNames _ =
        K "account-name" :*
        K "project-name" :*
        K "task-name" :*
        K "oneb-hours" :*
        K "oneb-count" :*
        K "oneb-median" :*
        K "prev-hours" :*
        K "prev-count" :*
        K "prev-median" :*
        K "curr-hours" :*
        K "curr-count" :*
        K "curr-median" :*
        Nil

    toColumns (TimereportsByTask a p t oneb prev curr) =
        f <$> toColumns oneb <*> toColumns prev <*> toColumns curr
      where
        f oneb' prev' curr' = append (I a :* I p :* I t :* Nil)
            (append oneb' (append prev' curr'))

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type TimereportsByTaskReport = Report
    "Hour marked by project/task"
    TimereportsByTaskParams
    [TimereportsByTask]

data TimereportsByTaskParams = TimereportsByTaskParams
    { tbtGenerated :: !UTCTime
    , tbtStartDay  :: !Day
    , tbtMidDay1   :: !Day
    , tbtMidDay2   :: !Day
    , tbtEndDay    :: !Day
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''TimereportsByTaskParams
deriveVia [t| ToJSON TimereportsByTaskParams   `Via` Sopica TimereportsByTaskParams |]
deriveVia [t| FromJSON TimereportsByTaskParams `Via` Sopica TimereportsByTaskParams |]
instance ToSchema TimereportsByTaskParams where declareNamedSchema = sopDeclareNamedSchema

instance ToHtml TimereportsByTaskParams where
    toHtmlRaw = toHtml
    toHtml TimereportsByTaskParams {..} = dl_ $ do
        dd_ "Generated at"
        dt_ $ toHtml $ show tbtGenerated

        dd_ "One before"
        dt_ $ toHtmlRaw $ show tbtStartDay <> " &mdash; " <> show (pred tbtMidDay1)

        dd_ "Previous month"
        dt_ $ toHtmlRaw $ show tbtMidDay1 <> " &mdash; " <> show (pred tbtMidDay2)

        dd_ "Current month"
        dt_ $ toHtmlRaw $ show tbtMidDay2 <> " &mdash; " <> show tbtEndDay

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

timereportsByTaskReport
    :: forall m env.
        ( PM.MonadTime m, MonadPersonio m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m TimereportsByTaskReport
timereportsByTaskReport = do
    now   <- currentTime
    today <- currentDay
    -- TODO: write date (not time!) handling lib
    let startDay = beginningOfPrev2Month today
    let midDay1  = beginningOfPrevMonth today
    let midDay2  = beginningOfCurrMonth today
    let endDay   = today
    let interval = startDay ... endDay
    -- Users
    fpm <- snd <$$> personioPlanmillMap
    -- Timereports
    trs <- concatMap toList <$>
        traverse (PMQ.timereports interval . view PM.identifier) (toList fpm)
    -- Group by task
    let trs1 = groupTimereports trs
    -- Group and sum
    trs2 <- itraverse (timereportsByTask midDay1 midDay2) trs1
    -- Sort
    let trs3 = sortBy (flip compare `on` _tbtHoursPrevMonth) $ toList trs2
    -- Result
    let params = TimereportsByTaskParams
          { tbtGenerated = now
          , tbtStartDay  = startDay
          , tbtMidDay1   = midDay1
          , tbtMidDay2   = midDay2
          , tbtEndDay    = endDay
          }
    pure $ Report params trs3

groupTimereports :: [PM.Timereport] -> Map PM.TaskId (NE.NonEmpty PM.Timereport)
groupTimereports = Map.unionsWith (<>) . map f
  where
    f tr = Map.singleton (PM.trTask tr) (tr NE.:| [])

timereportsByTask
    :: MonadPlanMillQuery m
    => Day
    -> Day
    -> PM.TaskId
    -> NE.NonEmpty PM.Timereport
    -> m TimereportsByTask
timereportsByTask midDay1 midDay2 taskId reports = do
    task <- PMQ.task taskId
    project <- traverse PMQ.simpleProject (PM.taskProject task)
    account <- traverse PMQ.account (project >>= (^. PM.pAccount))

    let mk p q c = TimereportsByTask
            { _tbtAccountName    = maybe "<unknown account>" PM.saName account
            , _tbtProjectName    = maybe "<unknown project>" (^. PM.pName) project
            , _tbtTaskName       = PM.taskName task
            , _tbtHoursOnebMonth = p
            , _tbtHoursPrevMonth = q
            , _tbtHoursCurrMonth = c
            }

    pure $ run reports $ mk
        <$> filtering isOnebMonth ftimereportStats
        <*> filtering isPrevMonth ftimereportStats
        <*> filtering isCurrMonth ftimereportStats
  where
    isOnebMonth r = PM.trStart r < midDay1
    isPrevMonth r = PM.trStart r >= midDay1 && PM.trStart r < midDay2
    isCurrMonth r = PM.trStart r >= midDay2

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

fsum :: Num a => L' a a
fsum = L' id (+) 0

flength :: L' a Int
flength = L' id (\x _ -> x + 1) 0

fmedian :: L' Double Double
fmedian = L'
    (fromMaybe 0 . TDigest.median)
    (flip TDigest.insert)
    (mempty :: TDigest.TDigest 100)

fmedian' :: Integral a => L' (NDT tu a) (NDT tu Double)
fmedian' = dimap (\(NDT x) -> fromIntegral x) NDT fmedian

ftimereportStats :: L' PM.Timereport TimereportsStats
ftimereportStats = lmap PM.trAmount f
  where
    f = TimereportsStats
        <$> rmap ndtConvert' fsum
        <*> flength
        <*> rmap (ndtConvert . fmap realToFrac) fmedian'
