{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Futuqu.Strm.Capacities (capacitiesStrm) where

import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time.Month
import Prelude ()

import qualified Servant.Types.SourceT as SourceT

import Futuqu.NT
import Futuqu.Rada.Capacities

capacitiesStrm
    :: ( MonadPlanMillQuery m, MonadPersonio m, MonadTime m, MonadMemoize m
       , Monad n)
    => Maybe Month
    -> (m :~> n)
    -> n (SourceT.SourceT n Capacity)
capacitiesStrm startingMonth' (NT runIn) = do
    (currMonth, ppm) <- runIn (Nothing :: Maybe ()) $ do
        currMonth <- dayToMonth <$> currentDay
        ppm <- personioPlanmillMap
        return (currMonth, ppm)

    -- a month below which we don't cache
    let cacheCutoffMonth = case currMonth of Month y _ -> Month (pred y) January

    -- we process one month at the time; streaming the response
    let go []         = SourceT.Stop
        go (month:ms) = SourceT.Effect $ do
            let interval = monthInterval month
            let cacheKey | month >= cacheCutoffMonth = Just month
                         | otherwise                 = Nothing
            trs <- runIn cacheKey $ capacitiesData' interval ppm
            return $ foldr SourceT.Yield (go ms) trs

    return $ SourceT.fromStepT $ go [ startingMonth .. currMonth ]
  where
    startingMonth = maybe jan2017 (max jan2017) startingMonth'
    jan2017 = Month 2017 January
