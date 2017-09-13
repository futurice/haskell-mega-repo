{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM.Report.Validation (validationReport) where

import Control.Concurrent.STM    (readTVarIO)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Ctx

import qualified Personio

validationReport :: Ctx -> IO (HtmlPage "validation-report")
validationReport ctx = do
    today <- currentDay

    validations0 <- liftIO $ readTVarIO $ ctxPersonioValidations ctx
    -- employees with some validation warnings
    let validations1 = filter (not . null . Personio._evMessages) validations0
    -- active only
    let validations = filter (isCurrentEmployee today) validations1

    pure $ page_ "Personio data validation" $ do
        row_ $ large_ 12 $ toHtml $
            show (length validations) ++ " employees:"

        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ "id"
                th_ "first"
                th_ "last"
                th_ "hire-date"
                th_ "end-date"
                th_ "warnings"

            tbody_ $ for_ validations $ \Personio.EmployeeValidation {..} -> tr_ $ do
                td_ $ toHtml $ show _evEmployeeId
                td_ $ toHtml _evFirst
                td_ $ toHtml _evLast
                td_ $ toHtml $ show _evHireDate
                td_ $ toHtml $ show _evEndDate
                td_ $ ul_ $ traverse_ (li_ . toHtml . show) _evMessages
  where
    isCurrentEmployee today v =
        maybe True (today <=) (v ^. Personio.evEndDate) &&
        maybe False (<= today) (v ^. Personio.evHireDate)
