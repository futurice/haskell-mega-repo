{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.HC.PersonioValidation (validationReport) where

import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

validationReport :: [P.EmployeeValidation] -> Day -> HtmlPage "personio-validation"
validationReport validations0 today = do
    let isActive p = P.employeeIsActive today p
            || (p ^. P.employeeStatus == P.Onboarding && maybe False ((>= today). addDays (-60)) (p ^. P.employeeHireDate))

    -- employees with some validation warnings
    let validations1 = filter (not . null . P._evMessages) validations0
    -- active only
    let validations2 = filter (isActive . P._evEmployee) validations1
    -- sort by starting day
    let validations = sortOn (Down . view P.employeeHireDate . P._evEmployee) validations2

    page_ "Personio data validation" $ do
        fullRow_ $ h1_ "Personio validations"

        fullRow_ $ div_ [ class_ "callout info" ] $ ul_ $ do
            li_ $ toHtml $ show (length validations) ++ " employees with incorrect or missing data:"
            li_ "Note: this report only checks data in Personio. For example it doesn't show if FUM login doesn't exist."

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
                td_ $ toHtml $ maybe "-" show $ e ^. P.employeeHireDate
                td_ $ toHtml $ maybe "-" show $ e ^. P.employeeEndDate
                td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeEmploymentType
                td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeContractType
                td_ $ showMessages msgs

showMessages :: Monad m => [P.ValidationMessage] -> HtmlT m ()
showMessages msgs = ul_ $ traverse_ (li_ . showMessage) msgs where
    showMessage (P.LoginInvalid _) = i_ "[IT] Invalid FUM Login."
    showMessage (P.EmailInvalid e) = do
        b_ "Invalid email: "
        toHtml e
        i_ " Primary email should be first.last@futurice.com"
    showMessage P.WorkPermitMissing = do
        b_ "Work permit is missing."
        i_ "Many don't need work permit, select explicit 'Not needed' for them."
    showMessage m = toHtml (show m)