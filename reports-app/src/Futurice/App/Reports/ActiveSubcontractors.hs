{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Reports.ActiveSubcontractors where

import Control.Monad             (filterM)
import Data.Fixed                (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (inf, sup, (...))
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data ActiveSubcontractor = ActiveSubcontractor
    { _asName  :: !P.Employee
    , _asDay   :: !Day
    , _asHours :: !(NDT 'Hours Centi)
    }
    deriving (Show, Generic)
    deriving anyclass (NFData)

deriveGeneric ''ActiveSubcontractor

instance ToSchema ActiveSubcontractor where declareNamedSchema = sopDeclareNamedSchema

data ActiveSubcontractorData = ActiveSubcontractorData
    { _asdInterval :: !(PM.Interval Day)
    , _asdData     :: !(Vector ActiveSubcontractor)
    }
    deriving (Show, Generic)
    deriving anyclass (NFData)

deriveGeneric ''ActiveSubcontractorData

instance ToSchema ActiveSubcontractorData where declareNamedSchema = sopDeclareNamedSchema

instance ToHtml ActiveSubcontractorData where
    toHtmlRaw = toHtml
    toHtml = toHtml . activeSubContractorRender

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

activeSubcontractor
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> P.Employee
    -> PM.User
    -> m (Vector ActiveSubcontractor)
activeSubcontractor interval emp user = do
    let uid = user ^. PM.identifier
    tr <- PMQ.timereports interval uid
    pure $ fmap toActiveSubcontractor tr
  where
      toActiveSubcontractor tr = ActiveSubcontractor
          { _asName = emp
          , _asDay = PM.trStart tr
          , _asHours = ndtConvert' $ PM.trAmount tr
          }

-- | Report for showing all subcontractors what marked hours in the spesified
-- interval
activeSubcontractorsReport
    :: (MonadPlanMillQuery m, MonadPersonio m, MonadTime m)
    => Day -> m ActiveSubcontractorData
activeSubcontractorsReport day = do
    let interval = beginningOfPrevMonth day ... endOfPrevMonth day
    fmp0 <- personioPlanmillMap
    fmp0' <- filterM getExternals $ HM.toList fmp0
    let fmp0'' = fmap snd fmp0'
    d <- V.concat <$> traverse (uncurry $ activeSubcontractor interval) fmp0''
    pure $ ActiveSubcontractorData interval d
  where
    getExternals (_, (_, planmillUser)) =
        case PM.uTeam planmillUser of
          Nothing -> pure False
          Just teamId -> do
              team <- PMQ.team teamId
              pure $ PM.tCostCenter team == Just 1190

activeSubContractorRender :: ActiveSubcontractorData -> HtmlPage "active-subcontractors"
activeSubContractorRender (ActiveSubcontractorData interval d) = page_ "Active Subcontractors" $ do
    let uniqueNames = length . nub . sort . V.toList $ fmap (view P.employeeFullname . _asName) d
    h1_ "Active Subcontractors"
    p_ $ do
        "generated from marked hours on "
        toHtml $ show $ inf interval
        " ... "
        toHtml $ show $ sup interval
    p_ $ do
        toHtml $ T.pack (show uniqueNames) <> " subcontractors marked hours during the interval"
    sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Name"
            th_ "Day"
            th_ "Hours"
        tbody_ $ for_ d $ \(ActiveSubcontractor name day hours) -> tr_ $ do
            td_ $ toHtml $ name ^. P.employeeFullname
            td_ $ toHtml $ show day
            td_ $ toHtml hours
