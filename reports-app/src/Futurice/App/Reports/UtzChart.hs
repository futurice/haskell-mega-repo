{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.UtzChart (utzChartData, utzChartRender) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                ((.=))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations
import Futurice.Monoid             (Average (..))
import Futurice.Time
import Numeric.Interval.NonEmpty   (Interval, inf, sup, (...))
import Servant.Chart               (Chart (..))

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C
import qualified PlanMill                      as PM
import qualified PlanMill.Queries              as PMQ

utzChartData
    :: forall m. ( MonadTime m, MonadPlanMillQuery m)
    => m (Map (Integer, Int) Double)
utzChartData = do 
    today <- currentDay
    uids <- view PM.identifier <$$> PMQ.users
    trs' <- bindForM (chopInterval $ interval today) $ \i ->
        traverse (PMQ.timereports i) uids
    let trs = trs' ^.. folded . folded . folded
    pure $ timereportUtzPerWeek trs
  where
    interval today = $(mkDay "2015-01-01") PM.... today

utzChartRender :: Map (Integer, Int) Double -> Chart "utz"
utzChartRender utzs = Chart . C.toRenderable $ do
    C.layout_title .= "UTZ per week"
    C.plot $ C.line "2018"
        [[(w, fromMaybe 0 $ utzs ^? ix (2018, w)) | w <- [1..53]]]
    C.plot $ C.line "2017"
        [[(w, fromMaybe 0 $ utzs ^? ix (2017, w)) | w <- [1..53]]]
    C.plot $ C.line "2016"
        [[(w, fromMaybe 0 $ utzs ^? ix (2016, w)) | w <- [1..53]]]
    C.plot $ C.line "2015"
        [[(w, fromMaybe 0 $ utzs ^? ix (2015, w)) | w <- [1..53]]]

-- bindForM and chopInterval used to cut the parallelism, as we ask "for everything"
-- TODO: move to integrations
bindForM :: Monad m => [a] -> (a -> m b) -> m [b]
bindForM [] _ = return []
bindForM (a:as) f = do
    b <- f a
    bs <- bindForM as f
    return (b : bs)

chopInterval :: (Ord a, Enum a) => Interval a -> [Interval a]
chopInterval i
    | s < 50    = [i]
    | otherwise = (mi ... md) : chopInterval (succ md ... ma)
  where
    mi = inf i
    md = toEnum (fromEnum mi + 50)
    ma = sup i
    s = fromEnum ma - fromEnum mi

timereportUtzPerWeek :: [PM.Timereport] -> Map (Integer, Int) Double
timereportUtzPerWeek = fmap getAverage . Map.fromListWith (<>) . fmap mk
  where
    mk :: PM.Timereport -> ((Integer, Int), Average Double)
    mk tr = ((y, w), tr ^. reportUtilizationAvg)
      where
        (y, w, _) = toWeekDate (PM.trStart tr)

-------------------------------------------------------------------------------
-- Copied from hours-api
-------------------------------------------------------------------------------

billableStatus :: Maybe PM.ProjectId -> Int -> EntryType
billableStatus Nothing 3 = EntryTypeOther
billableStatus _ 3       = EntryTypeNotBillable
billableStatus _ _       = EntryTypeBillable

reportUtilizationAvg :: Getter PM.Timereport (Average Double)
reportUtilizationAvg = getter $ \tr ->
    let NDT hours = ndtConvert' (PM.trAmount tr) :: NDT 'Hours Double
    in case billableStatus (PM.trProject tr) (PM.trBillableStatus tr) of
        EntryTypeBillable    -> Average hours 100
        EntryTypeNotBillable -> Average hours 0
        EntryTypeOther       -> mempty

data EntryType
    = EntryTypeBillable
    | EntryTypeNotBillable
    | EntryTypeOther
