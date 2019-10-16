{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.SupervisorsGraph (supervisorsGraph, Emp (..)) where

import Algebra.Graph.Class   (edges)
import Futurice.Company
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Tribe        (Tribe)
import Prelude ()
import Servant.Graph         (Graph (..), ToDotVertex (..))

import qualified Algebra.Graph.Export.Dot as Dot
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Personio                 as P

supervisorsGraph
    :: forall m. (MonadTime m, MonadPersonio m)
    => m (Graph Emp "supervisors")
supervisorsGraph = do
    today <- currentDay
    es0 <- P.personio P.PersonioEmployees
    let es1 = filter (T.isPrefixOf "Futu" . companyToText . P._employeeEmployer) es0
    let es = filter (P.employeeIsActive today) es1
    let m = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) es
    let g = edges (mapMaybe (f m) es)
    pure $ Graph g

  where
    f :: Map P.EmployeeId P.Employee -> P.Employee -> Maybe (Emp, Emp)
    f m e = do
        sid <- e ^. P.employeeSupervisorId
        s <- m ^? ix sid
        pure (toEmp s, toEmp e)

toEmp :: P.Employee -> Emp
toEmp e = Emp
    { empName     = e ^. P.employeeFullname
    , empTribe    = e ^. P.employeeTribe
    , empInternal = e ^. P.employeeEmploymentType == Just P.Internal
    }

data Emp = Emp
    { empName     :: !Text
    , empTribe    :: !Tribe
    , empInternal :: !Bool
    }
  deriving (Eq, Ord, Show, Generic)

instance NFData Emp

instance ToDotVertex Emp where
    exportVertexStyle = (Dot.defaultStyle (view lazy . empName))
        { Dot.vertexAttributes = va
        , Dot.graphAttributes =
            [ "rankdir" Dot.:= "LR"
            , "labelloc" Dot.:= "t"
            , "label" Dot.:= "Supervisor graph, based on Personio data"
            ]
        }
      where
        va (Emp _ t i) =
            [ "color" Dot.:= fromString (tribeToColour t)
            , "shape" Dot.:= if i then "oval" else "septagon"
            ]

-- @hashWithSalt 0@ is a way to extract the position in tribes.json :)
tribeToColour :: Tribe -> String
tribeToColour t = colours !! (hashWithSalt 0 t `mod` length colours)
  where
    colours =
        [ "#003441"
        , "#500A5A"
        , "#005A4B"
        , "#46289A"
        , "#210F00"
        , "#266826"
        , "#72625F"
        , "#7E878B"
        , "#BEC3E6"
        , "#FF5240" -- external,
        , "#CDECE4" -- no tribe
        ]
