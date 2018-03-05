{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.HC.PersonioValidation (validationReport) where

import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

import Futurice.App.HC.Markup

validationReport :: [P.EmployeeValidation] -> Day -> HtmlPage "personio-validation"
validationReport validations0 today = do
    let isActive p = P.employeeIsActive today p
            || (p ^. P.employeeStatus == P.Onboarding && maybe False (>= addDays (-60) today) (p ^. P.employeeHireDate))

    -- employees with some validation warnings
    let validations1 = filter (not . null . P._evMessages) validations0
    -- active only
    let validations2 = filter (isActive . P._evEmployee) validations1
    -- sort by starting day
    let validations = sortOn (Down . view P.employeeHireDate . P._evEmployee) validations2

    page_ "Personio data validation" (Just NavPersonioValidation) $ do
        ul_ $ do
            li_ $ toHtml $ show (length validations) ++ " employees with incorrect or missing data:"
            li_ "Note: this report only checks data in Personio. For example it doesn't show if FUM login doesn't exist."
            li_ $ "Only " <> em_ "Active" <> " and " <> em_ "Onboarding" <> " (hire date >= today - 60 days) people are shown"
            li_ $ do
                "There are separate lists for "
                a_ [ href_ "#employees" ] "Employees"
                " and "
                a_ [ href_ "#externals" ] "Externals"

        fullRow_ $ a_ [ name_ "employees" ] $ h2_ "Employees"
        fullRow_ $ showValidations $ filter isInternal validations

        fullRow_ $ a_ [ name_ "externals" ] $ h2_ "Externals"
        fullRow_ $ showValidations $ filter (not . isInternal) validations
  where
    isInternal :: P.EmployeeValidation -> Bool
    isInternal v = Just P.Internal == v ^. P.evEmployee . P.employeeEmploymentType


showValidations :: (Foldable f, Monad m)  => f P.EmployeeValidation -> HtmlT m ()
showValidations validations = fullRow_ $ table_ $ do
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
    showMessage (P.LoginInvalid _) = b_ "IT: " <> i_ "Invalid FUM Login."
    showMessage P.WorkPhoneMissing = b_ "IT: " <> i_ "Work phone is missing."
    showMessage (P.EmailInvalid e) = do
        b_ "Invalid email: "
        toHtml e
        i_ " Primary email should be first.last@futurice.com"
    showMessage P.WorkPermitMissing = do
        b_ "Work permit is missing."
        " "
        i_ "Many don't need work permit, select explicit 'Not needed' for them."
    showMessage m = toHtml (show m)
