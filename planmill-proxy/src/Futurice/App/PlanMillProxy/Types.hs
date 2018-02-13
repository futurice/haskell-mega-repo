{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.PlanMillProxy.Types (
    Ctx (..),
    Stats (..),
    ) where

import Futurice.Generics
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant  (Cache)
import PlanMill          (Cfg)
import PlanMill.Worker   (Workers)
import Prelude ()

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !Cache
    , ctxPlanmillCfg  :: !Cfg
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogger       :: !Logger
    , ctxWorkers      :: !Workers
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

data Stats = Stats
    { statsCachedAvgAge      :: Double
    , statsCachedMinAge      :: Double
    , statsCachedMaxAge      :: Double
    , statsCachedTotal       :: Int
    , statsTimereportsAvgAge :: Double
    , statsTimereportsMinAge :: Double
    , statsTimereportsMaxAge :: Double
    , statsTimereportsTotal  :: Int
    }
  deriving Show

deriveGeneric ''Stats

deriveVia [t| ToJSON Stats   `Via` Sopica Stats |]
deriveVia [t| FromJSON Stats `Via` Sopica Stats |]

instance ToSchema Stats where
    declareNamedSchema = sopDeclareNamedSchema
