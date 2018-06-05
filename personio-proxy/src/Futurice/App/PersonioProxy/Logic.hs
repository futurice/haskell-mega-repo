{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.PersonioProxy.Logic where

import Control.Concurrent.STM           (readTVarIO)
import Control.Lens                     ((.=))
import Data.Aeson.Types                 (parseEither, parseJSON)
import Data.Distributive                (Distributive (..))
import Data.Functor.Rep                 (Representable (..), distributeRep)
import Data.List                        (partition)
import Futurice.App.PersonioProxy.Types
import Futurice.Cache                   (cached)
import Futurice.Prelude
import Prelude ()
import Servant
import Servant.Chart                    (Chart (..))

import qualified Futurice.Chart.Stacked        as C
import qualified Futurice.Postgres             as Postgres
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Personio                      as P

personioRequest :: Ctx -> P.SomePersonioReq -> Handler P.SomePersonioRes
personioRequest ctx (P.SomePersonioReq res) = case res of
    P.PersonioEmployees   -> P.SomePersonioRes res <$> rawEmployees ctx
    P.PersonioValidations -> P.SomePersonioRes res <$> rawValidations ctx
    P.PersonioAll         -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (P.SomePersonioRes res (es, vs))

rawValidations :: Ctx -> Handler [P.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx

rawEmployees :: Ctx -> Handler [P.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es

scheduleEmployees :: Ctx -> Handler [P.ScheduleEmployee]
scheduleEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ P.fromPersonio $ toList es

-------------------------------------------------------------------------------
-- Employees Chart
-------------------------------------------------------------------------------

employeesChart :: Ctx -> Handler (Chart "employees")
employeesChart ctx = do
    values <- liftIO $ runLogT "chart-employees" (ctxLogger ctx) $
        cached (ctxCache ctx) 600 () $ do
            res <- Postgres.safePoolQuery_ ctx "SELECT timestamp, contents FROM \"personio-proxy\".log;"
            return $ res <&> \(t, c) ->
                let d = utctDay t
                in (d, employeesCounts d c)

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Active employees"
        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (PerEmploymentType "Internal" "External")
            values
  where
    employeesCounts :: Day -> Value -> PerEmploymentType Int
    employeesCounts today v = case parseEither parseJSON v of
        Left _    -> PerEmploymentType 0 0
        -- TODO: use @folds@ -package
        Right es' -> PerEmploymentType (length int) (length ext)
          where
            es = filter (P.employeeIsActive today) es'
            (int, ext) = partition (\p -> p ^. P.employeeEmploymentType /= Just P.External) es

data PerEmploymentType a = PerEmploymentType a a
  deriving (Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (PerEmploymentType a)

instance Distributive PerEmploymentType where
    distribute = distributeRep

instance Representable PerEmploymentType where
    type Rep PerEmploymentType = P.EmploymentType

    tabulate f = PerEmploymentType (f P.Internal) (f P.External)
    index (PerEmploymentType x _) P.Internal = x
    index (PerEmploymentType _ x) P.External = x
