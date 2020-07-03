{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.FumAbsences where

import Futurice.Generics
import Futurice.IdMap        (key)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Data.Vector      as V
import qualified FUM
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data FumAbsences = FumAbsences
    { _fumAbsencesUsername :: !(FUM.Login)
    , _fumAbsencesMonth    :: !Month
    , _fumAbsencesAbsences :: ![Day]
    } deriving (GhcGeneric, ToSchema, SopGeneric, HasDatatypeInfo, NFData)
      deriving ToJSON via (Sopica FumAbsences)

fumAbsences :: (Monad m, MonadPlanMillQuery m, MonadPersonio m) => FUM.Login -> Month -> m FumAbsences
fumAbsences username month = do
    let startDay = firstDayOfMonth month
    let endDay   = lastDayOfMonth month
    fpm <- personioPlanmillMap
    case ((^. key) . snd) <$> fpm ^.at username of
      Nothing -> error "PlanMill user not found"
      Just pid -> do
          as0 <- PMQ.absences
          let as1 = V.filter (\ab -> PM.absenceStart ab <= endDay && PM.absenceFinish ab >= startDay ) $ V.filter (\ab -> PM.absencePerson ab == pid) as0
          let days = nub $ sort $ filter (\d -> d >= startDay && d <= endDay) $ concat $ fmap (\ab -> [PM.absenceStart ab .. PM.absenceFinish ab] ) $ V.toList as1
          pure $ FumAbsences
              { _fumAbsencesUsername = username
              , _fumAbsencesMonth = month
              , _fumAbsencesAbsences = days
              }
