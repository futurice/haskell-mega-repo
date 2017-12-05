{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM.Report.Validation (validationReport) where

import Control.Concurrent.STM    (readTVarIO)
import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Ctx

import qualified Personio as P

validationReport :: Ctx -> IO (HtmlPage "validation-report")
validationReport ctx = do
    now <- currentTime
    today <- currentDay

    let isActive p = P.employeeIsActive now p
            || (p ^. P.employeeStatus == P.Onboarding && maybe False ((>= today). addDays (-60)) (p ^. P.employeeHireDate))

    validations0 <- liftIO $ readTVarIO $ ctxPersonioValidations ctx
    -- employees with some validation warnings
    let validations1 = filter (not . null . P._evMessages) validations0
    -- active only
    let validations2 = filter (isActive . P._evEmployee) validations1
    -- sort by starting day
    let validations = sortOn (Down . view P.employeeHireDate . P._evEmployee) validations2

    pure $ page_ "Personio data validation" $ do
        row_ $ large_ 12 $ toHtml $
            show (length validations) ++ " employees:"

        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "id"
                th_ "name"
                th_ "fum"
                th_ "status"
                th_ "hire-date"
                th_ "end-date"
                th_ "internal"
                th_ "type"
                th_ "warnings"

            tbody_ $ for_ validations $ \(P.EmployeeValidation e msgs) -> tr_ $ do
                td_ $ toHtml $ e ^. P.employeeId
                td_ $ toHtml $ e ^. P.employeeFullname
                td_ $ traverse_ toHtml $ e ^. P.employeeLogin
                td_ $ toHtml $ e ^. P.employeeStatus
                td_ $ toHtml $ show $ e ^. P.employeeHireDate
                td_ $ toHtml $ show $ e ^. P.employeeEndDate
                td_ $ toHtml $ show $ e ^. P.employeeEmploymentType
                td_ $ toHtml $ show $ e ^. P.employeeContractType
                td_ $ ul_ $ traverse_ (li_ . toHtml . show) msgs
