{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM.Report.Validation (validationReport) where

import Control.Concurrent.STM    (readTVarIO)
import Data.Ord                  (Down (..))
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Ctx

import qualified Personio

validationReport :: Ctx -> IO (HtmlPage "validation-report")
validationReport ctx = do
    now <- currentTime

    validations0 <- liftIO $ readTVarIO $ ctxPersonioValidations ctx
    -- employees with some validation warnings
    let validations1 = filter (not . null . Personio._evMessages) validations0
    -- active only
    let validations2 = filter (Personio.employeeIsActive now . Personio._evEmployee) validations1
    -- sort by starting day
    let validations = sortOn (Down . view Personio.employeeHireDate . Personio._evEmployee) validations2

    pure $ page_ "Personio data validation" $ do
        row_ $ large_ 12 $ toHtml $
            show (length validations) ++ " employees:"

        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "id"
                th_ "name"
                th_ "fum"
                th_ "hire-date"
                th_ "end-date"
                th_ "internal"
                th_ "type"
                th_ "warnings"

            tbody_ $ for_ validations $ \(Personio.EmployeeValidation e msgs) -> tr_ $ do
                td_ $ toHtml $ e ^. Personio.employeeId
                td_ $ toHtml $ e ^. Personio.employeeFullname
                td_ $ traverse_ toHtml $ e ^. Personio.employeeLogin
                td_ $ toHtml $ show $ e ^. Personio.employeeHireDate
                td_ $ toHtml $ show $ e ^. Personio.employeeEndDate
                td_ $ toHtml $ show $ e ^. Personio.employeeEmploymentType
                td_ $ toHtml $ show $ e ^. Personio.employeeContractType
                td_ $ ul_ $ traverse_ (li_ . toHtml . show) msgs
