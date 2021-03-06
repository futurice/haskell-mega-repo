{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.PowerAllRevenues where

import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time.Month   (Month (..))
import Prelude ()

import qualified Data.Map         as M
import qualified Data.Text        as T
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data PowerAllRevenues = PowerAllRevenues
    { powerAllRevenuesMonth :: !Month
    , powerAllRevenuesData  :: !(Map Text [PowerAllRevenuesRow]) -- TODO: give more proper type for the key
    }
    deriving (GhcGeneric, SopGeneric)
    deriving anyclass (NFData, HasDatatypeInfo, ToSchema)
    deriving (FromJSON, ToJSON) via (Sopica PowerAllRevenues)

data PowerAllRevenuesRow = PowerAllRevenuesRow
    { powerAllRevenuesRowCustomer          :: !Text
    , powerAllRevenuesRowProject           :: !Text
    , powerAllRevenuesRowProjectId         :: !(Maybe PM.ProjectId)
    , powerAllRevenuesRowValueCreation     :: !(Maybe Double) -- TODO: Think of better type for all these
    , powerAllRevenuesRowActualRevenue     :: !(Maybe Double)
    , powerAllRevenuesRowInvoicedNoVat     :: !(Maybe Double)
    , powerAllRevenuesRowActualHours       :: !(Maybe Double)
    , powerAllRevenuesRowActualNonBillable :: !(Maybe Double)
    , powerAllRevenuesRowSalesPrice        :: !(Maybe Double)
    , powerAllRevenuesRowEffectivePrice    :: !(Maybe Double)
    , powerAllRevenuesRowInvoiceNumber     :: !(Maybe Int)
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo, NFData, ToSchema)
      deriving (FromJSON, ToJSON) via (Sopica PowerAllRevenuesRow)

powerAllRevenuesReport :: (MonadTime m, MonadPlanMillQuery m) => Maybe Month -> m PowerAllRevenues
powerAllRevenuesReport mmonth = do
    month <- maybe currentMonth pure mmonth
    report <- PMQ.allRevenuesReport (monthYear month) (toInteger $ fromEnum $ monthName month)
    projects <- PMQ.projects
    let operationalMap = M.fromList $ fmap (\p -> (PM._pOperationalId p, p)) $ toList projects
    let report' = M.toList $ PM.getRevenuesData report
        report'' = fmap (toPowerAllRevenuesRow operationalMap) report'
    pure $ PowerAllRevenues month $ M.fromList report''
  where
    toPowerAllRevenuesRow operationalMap (portfolioName, portfolios) =
        let tribe = keyName $ T.splitOn "-" portfolioName
            keyName [_costcenter, name] = let n = T.strip name in if "pass through invoicing" `T.isSuffixOf` n then T.dropEnd 23 n else n
            keyName xs = fold xs
            row p = PowerAllRevenuesRow
                { powerAllRevenuesRowCustomer          = PM._arpCustomer p
                , powerAllRevenuesRowProject           = PM._arpProject p
                , powerAllRevenuesRowProjectId         = PM._pId <$> (operationalMap ^.at (PM._arpOperationalId p))
                , powerAllRevenuesRowValueCreation     = PM._arpValueCreation p
                , powerAllRevenuesRowActualRevenue     = PM._arpActualRevenue p
                , powerAllRevenuesRowInvoicedNoVat     = PM._arpInvoicedNoVat p
                , powerAllRevenuesRowActualHours       = PM._arpActualHours p
                , powerAllRevenuesRowActualNonBillable = PM._arpActualNonBillable p
                , powerAllRevenuesRowSalesPrice        = PM._arpSalesPrice p
                , powerAllRevenuesRowEffectivePrice    = PM._arpEffectivePrice p
                , powerAllRevenuesRowInvoiceNumber     = PM._arpInvoiceNumber p
                }
        in (tribe, fmap row portfolios)
