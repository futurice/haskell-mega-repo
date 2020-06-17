{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.Capacity where

import Futurice.Generics
import Futurice.IdMap
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time         (NDT (..))
import Futurice.Time.Month   (monthInterval)
import Prelude ()

import qualified Data.Vector      as V
import qualified FUM.Types.Login  as FUM
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data Capacity = Capacity
    { _capDay      :: !Day
    , _capHasCapacity :: !Bool
    } deriving (GhcGeneric, ToSchema, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica Capacity)

hasCapacity :: (MonadPlanMillQuery m, MonadPersonio m, MonadTime m) => FUM.Login -> Month -> m [Capacity]
hasCapacity login month = do
    emap <- personioPlanmillMap
    let Just (_, planmillUser) = emap ^.at login
    capacity <- PMQ.capacities (monthInterval month) (planmillUser ^. key)
    pure $ sortOn _capDay $ fmap toCapacity $ V.toList capacity
  where
    toCapacity ucap = Capacity
        { _capDay = PM.userCapacityDate ucap
        , _capHasCapacity = PM.userCapacityAmount ucap > NDT 0
        }
