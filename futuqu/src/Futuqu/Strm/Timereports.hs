{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Futuqu.Strm.Timereports (timereportsStrm) where

import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time.Month
import Prelude ()

import qualified Data.Text             as T
import qualified PlanMill.Queries      as PMQ
import qualified Servant.Types.SourceT as SourceT

import Futuqu.Rada.Timereports

timereportsStrm
    :: ( MonadPlanMillQuery m, MonadPersonio m, MonadTime m, MonadMemoize m
       , Monad n)
    => Maybe Month
    -> (forall a. m a -> n a)
    -> n (SourceT.SourceT n Timereport)
timereportsStrm startingMonth' runIn = do
    (currMonth, users, sts, bss) <- runIn $ do
        currMonth <- dayToMonth <$> currentDay
        users <- PMQ.users
        sts <- fmap2 T.toLower $ PMQ.allEnumerationValues Proxy Proxy
        bss <- fmap2 T.toLower $ PMQ.allEnumerationValues Proxy Proxy
        -- we could ask for all tasks in advance, but
        -- that we'll delay initial response
        return (currMonth, users, sts, bss)

    -- we process one month at the time; streaming the response
    let go []         = SourceT.Stop
        go (month:ms) = SourceT.Effect $ do
            let interval = monthInterval month
            trs <- runIn $ timereportsData' interval users sts bss
            return $ foldr SourceT.Yield (go ms) trs

    return $ SourceT.fromStepT $ go [ startingMonth .. currMonth ]
  where
    startingMonth = maybe jan2015 (max jan2015) startingMonth'
    jan2015 = Month 2015 January


    fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
    fmap2 = fmap . fmap
