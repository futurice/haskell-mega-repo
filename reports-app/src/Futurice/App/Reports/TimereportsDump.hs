{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.TimereportsDump (
    SimpleTimereport (..),
    timereportsDump,
    ) where

import Control.Monad.State.Lazy  (StateT, execStateT, modify)
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
    let interval = $(mkDay "2018-08-01") PM.... today
    let chopped = chopInterval interval
    us <- PMQ.users
    consume $ bindForM_ (toList us) $ \pmUser -> do
        trs <- lift $ for chopped $ \i -> do
            -- we ask for all timereports
            PMQ.timereports i (pmUser ^. PM.identifier)
        for_ (PM.userLogin pmUser) $ \login -> do
            for_ (mconcat trs) $ \tr -> do
                for_ (PM.trProject tr) $ \prId -> do
                    (p, t) <- lift $ liftA2 (,)
                        (PMQ.project prId)
                        (PMQ.task $ PM.trTask tr)
                    produce SimpleTimereport
                        { strLogin    = login
                        , strProject  = PM.pName p
                        , strTask     = PM.taskName t
                        , strDay      = PM.trStart tr
                        , strDuration = ndtConvert' $ PM.trAmount tr
                        , strComment  = fromMaybe mempty $ PM.trComment tr
                        }
  where
    consume :: Monad n => StateT ([a] -> [a]) n () -> n [a]
    consume action = ($[]) <$> execStateT action id

    produce :: Monad n => a -> StateT ([a] -> [a]) n ()
    produce x = modify (. (x :))

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- bindForM and chopInterval used to cut the parallelism, as we ask "for everything"
bindForM_ :: Monad m => [a] -> (a -> m b) -> m ()
bindForM_ [] _ = return ()
bindForM_ (a:as) f = do
    _ <- f a
    bindForM_ as f

chopInterval :: (Ord a, Enum a) => Interval a -> [Interval a]
chopInterval i
    | s < 50    = [i]
    | otherwise = (mi ... md) : chopInterval (succ md ... ma)
  where
    mi = inf i
    md = toEnum (fromEnum mi + 50)
    ma = sup i
    s = fromEnum ma - fromEnum mi
