{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.TimereportsDump (
    SimpleTimereport (..),
    timereportsDump,
    ) where

import Data.Aeson                (ToJSON (..))
import FUM.Types.Login           (Login)
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import qualified Data.Csv         as Csv
import qualified Data.Swagger     as Sw
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data SimpleTimereport = SimpleTimereport
    { strLogin    :: !Login
    , strProject  :: !Text
    , strTask     :: !Text
    , strDay      :: !Day
    , strDuration :: !(NDT 'Hours Double)
    , strComment  :: !Text
    }
  deriving Generic

instance NFData SimpleTimereport

instance Csv.DefaultOrdered SimpleTimereport
instance Csv.ToNamedRecord SimpleTimereport

instance ToJSON SimpleTimereport
instance Sw.ToSchema SimpleTimereport

timereportsDump
    :: forall m. ( MonadTime m, MonadPlanMillQuery m)
    => m [SimpleTimereport]
timereportsDump = do
    today <- currentDay
    let interval = $(mkDay "2017-01-01") PM.... today
    let chopped = chopInterval interval
    us <- PMQ.users
    data_ <- bindForM chopped $ \i ->
        for (toList us) $ \pmUser -> do
            for (PM.userLogin pmUser) $ \login -> do
                -- we ask for all timereports
                trs <- PMQ.timereports i (pmUser ^. PM.identifier)
                for trs $ \tr -> do
                    for (PM.trProject tr) $ \prId -> do
                        (p, t) <- liftA2 (,)
                            (PMQ.project prId)
                            (PMQ.task $ PM.trTask tr)
                        return SimpleTimereport
                            { strLogin    = login
                            , strProject  = PM.pName p
                            , strTask     = PM.taskName t
                            , strDay      = PM.trStart tr
                            , strDuration = ndtConvert' $ PM.trAmount tr
                            , strComment  = fromMaybe mempty $ PM.trComment tr
                            }

    return $ data_ ^.. folded  . folded . folded . folded . folded

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- bindForM and chopInterval used to cut the parallelism, as we ask "for everything"
bindForM :: Monad m => [a] -> (a -> m b) -> m [b]
bindForM xs f = go xs where
    go [] = return []
    go (a:as) = do
        b <- f a
        (b:) <$> go as

chopInterval :: (Ord a, Enum a) => Interval a -> [Interval a]
chopInterval i
    | s < 50    = [i]
    | otherwise = (mi ... md) : chopInterval (succ md ... ma)
  where
    mi = inf i
    md = toEnum (fromEnum mi + 50)
    ma = sup i
    s = fromEnum ma - fromEnum mi
