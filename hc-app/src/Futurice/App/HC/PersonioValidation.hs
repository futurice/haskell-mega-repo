{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.PersonioValidation (validationReport) where

import Data.Ord         (Down (..))
import Data.Time        (addDays)
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

import Futurice.App.HC.Markup

-- | TODO move this checks to personio-client
data Consistency = ActiveAfterContractEndDate
  deriving Show

checkConsistency :: Day -> P.Employee -> [Consistency]
checkConsistency today e = catMaybes
    [ check ActiveAfterContractEndDate $
        e ^. P.employeeStatus == P.Active && maybe False (< today) (e ^. P.employeeEndDate)
    ]
  where
    check x True  = Just x
    check _ False = Nothing

data V = V
    { vEmployee    :: P.Employee
    , _vConsistency :: [Consistency]
    , _vValidations :: [P.ValidationMessage]
    }

toV :: Day -> P.EmployeeValidation -> V
toV today (P.EmployeeValidation e msgs) =
    V e (checkConsistency today e) msgs

isOkV :: V -> Bool
isOkV (V _ xs ys) = null xs && null ys

validationReport :: [P.EmployeeValidation] -> Day -> HtmlPage "personio-validation"
validationReport validations0 today = do
    page_ "Personio data validation" (Just NavPersonioValidation) $ do
        ul_ $ do
            li_ $ toHtml $ unwords
                [ show (length intValidations)
                , "employees and"
                , show (length extValidations)
                , "externals with incorrect or missing data:"
                ]
            li_ "Note: this report only checks data in Personio. For example it doesn't show if FUM login doesn't exist."
            li_ $ "Only " <> em_ "Active" <> " and " <> em_ "Onboarding" <> " (hire date >= today - 60 days) people are shown"
            li_ $ do
                "There are separate lists for "
                a_ [ href_ "#employees" ] "Employees"
                " and "
                a_ [ href_ "#externals" ] "Externals"

        fullRow_ $ a_ [ name_ "employees" ] $ h2_ "Employees"
        fullRow_ $ showValidations intValidations

        fullRow_ $ a_ [ name_ "externals" ] $ h2_ "Externals"
        fullRow_ $ showValidations extValidations
  where
    isInternal :: V -> Bool
    isInternal (V e _ _) = Just P.Internal == e ^. P.employeeEmploymentType

    intValidations = filter isInternal validations
    extValidations = filter (not . isInternal) validations

    -- don't check contract end dates here
    isActive p = p ^. P.employeeStatus == P.Active
        || p ^. P.employeeStatus == P.Leave
        || (p ^. P.employeeStatus == P.Onboarding && maybe False (>= addDays (-60) today) (p ^. P.employeeHireDate))

    -- employees with some validation warnings
    validations1 = filter (not . isOkV) $ map (toV today) validations0
    -- active only
    validations2 = filter (isActive . vEmployee) validations1
    -- sort by starting day
    validations = sortOn (Down . view P.employeeHireDate . vEmployee) validations2

showValidations :: (Foldable f, Monad m)  => f V -> HtmlT m ()
showValidations validations = fullRow_ $ table_ $ do
    thead_ $ tr_ $ do
        th_ "id"
        th_ "name"
        th_ "fum"
        th_ "status"
        th_ "tribe"
        th_ "hire-date"
        th_ "end-date"
        th_ "internal"
        th_ "type"
        th_ "warnings"

    tbody_ $ for_ validations $ \(V e cs msgs) -> tr_ $ do
        td_ $ toHtml $ e ^. P.employeeId
        td_ $ toHtml $ e ^. P.employeeFullname
        td_ $ traverse_ toHtml $ e ^. P.employeeLogin
        td_ $ toHtml $ e ^. P.employeeStatus
        td_ $ toHtml $ e ^. P.employeeTribe
        td_ $ toHtml $ maybe "-" show $ e ^. P.employeeHireDate
        td_ $ toHtml $ maybe "-" show $ e ^. P.employeeEndDate
        td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeEmploymentType
        td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeContractType
        td_ $ showMessages cs msgs

showMessages :: Monad m => [Consistency] -> [P.ValidationMessage] -> HtmlT m ()
showMessages cons msgs = ul_ $ do
    traverse_ (li_ . showConsistency) cons
    traverse_ (li_ . showMessage) msgs
  where
    showConsistency = toHtml . show

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
