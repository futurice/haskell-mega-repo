{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
module Futurice.App.PersonioProxy.Types where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (Cache)
import Futurice.Generics
import Futurice.Postgres      (Connection, HasPostgresPool (..), Pool)
import Futurice.Prelude
import Prelude ()

import qualified Personio

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxLogger          :: !Logger
    , ctxCache           :: !Cache
    , ctxManager         :: !Manager
    , ctxConfig          :: !Personio.Cfg
    , ctxPostgres        :: !(Pool Connection)
    , ctxPersonioData    :: !(TVar Personio.PersonioAllData)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres

data AttritionRate = AttritionRate
    { _attrAttritionRate :: !Double
    , _attrLeavers       :: !Int
    , _attrMonths        :: !(Map Month Int)
    } deriving (Generic, ToSchema, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica AttritionRate)

data MonthlyCompensation = MonthlyCompensation
    { _mcTotal     :: !Double
    , _mcPerOffice :: !(Map Text Double)
    } deriving (Generic, ToSchema, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica MonthlyCompensation)
