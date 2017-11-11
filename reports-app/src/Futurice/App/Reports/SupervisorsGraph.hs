{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.SupervisorsGraph (supervisorsGraph) where

import Algebra.Graph.Class   (edges)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Graph         (Graph (..))

import qualified Data.Map as Map
import qualified Personio as P

supervisorsGraph
    :: forall m. (MonadTime m, MonadPersonio m)
    => m (Graph "supervisors")
supervisorsGraph = do
    now <- currentTime
    es0 <- P.personio P.PersonioEmployees
    let es = filter (P.employeeIsActive now) es0
    let m = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) es
    let g = edges (mapMaybe (f m) es)
    pure $ Graph g

  where
    f :: Map P.EmployeeId P.Employee -> P.Employee -> Maybe (Text, Text)
    f m e = do
        sid <- e ^. P.employeeSupervisorId
        s <- m ^? ix sid
        pure (s ^. P.employeeFullname, e ^. P.employeeFullname)
