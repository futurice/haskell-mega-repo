{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Reports.TimereportsDump (
    SimpleTimereport (..),
    timereportsDump,
    ) where

import Data.Aeson                (ToJSON (..))
import FUM.Types.Login           (Login)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Swagger        as Sw
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data SimpleTimereport = SimpleTimereport
    { strLogin    :: !Login
    , strPlanmill :: !PM.UserId
    , strPersonio :: !P.EmployeeId
    , strProject  :: !Text
    , strTask     :: !Text
    , strDay      :: !Day
    , strDuration :: !(NDT 'Hours Double)
    , strComment  :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''SimpleTimereport

instance Csv.DefaultOrdered SimpleTimereport where
    headerOrder = Csv.genericHeaderOrder $ Csv.defaultOptions
        { Csv.fieldLabelModifier = drop 3
        }
instance Csv.ToNamedRecord SimpleTimereport where
    toNamedRecord = Csv.genericToNamedRecord $ Csv.defaultOptions
        { Csv.fieldLabelModifier = drop 3
        }

instance ToJSON SimpleTimereport
instance Sw.ToSchema SimpleTimereport where declareNamedSchema = sopDeclareNamedSchema

timereportsDump
    :: forall m. (MonadTime m, MonadPlanMillQuery m, MonadPersonio m)
    => m [SimpleTimereport]
timereportsDump = do
    today <- currentDay
    let interval = $(mkDay "2017-01-01") PM.... today
    let chopped = chopInterval interval
    fpm0 <- personioPlanmillMap
    let fpm = HM.filter (P.employeeIsActive today . fst) fpm0
    data_ <- bindForM chopped $ \i ->
        ifor fpm $ \login (e, pmUser) -> do
            -- we ask for all timereports
            let pmUid = pmUser ^. PM.identifier
            trs <- PMQ.timereports i pmUid
            for trs $ \tr -> do
                for (PM.trProject tr) $ \prId -> do
                    (p, t) <- liftA2 (,)
                        (PMQ.project prId)
                        (PMQ.task $ PM.trTask tr)
                    return SimpleTimereport
                        { strLogin    = login
                        , strPlanmill = pmUid
                        , strPersonio = e ^. P.employeeId
                        , strProject  = PM._pName p
                        , strTask     = PM.taskName t
                        , strDay      = PM.trStart tr
                        , strDuration = ndtConvert' $ PM.trAmount tr
                        , strComment  = fromMaybe mempty $ PM.trComment tr
                        }

    return $ data_ ^.. folded  . folded . folded . folded

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
