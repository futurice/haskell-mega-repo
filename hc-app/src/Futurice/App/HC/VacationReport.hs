{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.VacationReport where

import Data.Aeson       (object, (.=))
import Futurice.Prelude
import Prelude ()
import Text.Microstache (Template, compileMustacheText, renderMustache)

import Futurice.App.HC.API
import Futurice.App.HC.Markup

import qualified Data.Map    as Map
import qualified Data.Text   as T
import qualified Data.Vector as V
import qualified Personio    as P
import qualified PlanMill    as PM

data Holiday = Holiday
    { annualHoliday     :: !Double
    , usedAnnualHoliday :: !Double
    } deriving (Show)

data PersonHoliday = PersonHoliday
    { employeeId        :: !P.EmployeeId
    , employeeFirstName :: !Text
    , holidays          :: !(Map (Maybe Int) Holiday)
    } deriving (Show)

renderReport :: PM.EarnedVacations -> [P.Employee] -> HtmlPage "vacation-report"
renderReport earnedVacations pemployees = page_ "German vacation report" (Just NavVacationReport) $ do
    h2_ "People that are not found in Personio"
    sortableTable_ $ do
        thead_ $ do
            th_ "Name"
            th_ "HR number"
            th_ "Year"
            th_ "Annual holiday"
            th_ "Used annual holiday"
            th_ "Annual holidays remaining"
            th_ "Bonus holiday"
            th_ "Bonus holiday remaining"
            th_ "Other annual holiday"
            th_ "Saved leaves"
            th_ "Saved leaves remaining"
        tbody_ $ do
            for_ (fst vacationPart) $ \row -> tr_ $ do
                td_ $ toHtml $ PM._vacationUserName row
                td_ $ toHtml $ maybe "" textShow $ PM._vacationUserHRNumber row
                td_ $ toHtml $ maybe "" textShow $ PM._vacationYear row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationUsedAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHolidaysRemaining row
                td_ $ toHtml $ textShow $ PM._vacationBonusHoliday row
                td_ $ toHtml $ textShow $ PM._vacationBonusHolidayRemaining row
                td_ $ toHtml $ textShow $ PM._vacationOtherAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeave row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeaveRemaining row
    h2_ "People found in Personio"
    sortableTable_ $ do
        thead_ $ do
            th_ "Name"
            th_ "HR number"
            th_ "Year"
            th_ "Annual holiday"
            th_ "Used annual holiday"
            th_ "Annual holidays remaining"
            th_ "Bonus holiday"
            th_ "Bonus holiday remaining"
            th_ "Other annual holiday"
            th_ "Saved leaves"
            th_ "Saved leaves remaining"
            th_ ""
        tbody_ $ do
            for_ (snd vacationPart) $ \(row,eid) -> tr_ $ do
                td_ $ toHtml $ PM._vacationUserName row
                td_ $ toHtml $ maybe "" textShow $ PM._vacationUserHRNumber row
                td_ $ toHtml $ maybe "" textShow $ PM._vacationYear row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationUsedAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHolidaysRemaining row
                td_ $ toHtml $ textShow $ PM._vacationBonusHoliday row
                td_ $ toHtml $ textShow $ PM._vacationBonusHolidayRemaining row
                td_ $ toHtml $ textShow $ PM._vacationOtherAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeave row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeaveRemaining row
                td_ $ a_ [ style_ "display: block", recordHref_ recVacationReportEmail eid] $ toHtml ("Show email" :: Text)
  where
      vacationPart = foldl (\s row ->
                              case Map.lookup (planmillNameToPersonio $ PM._vacationUserName row) personioNameMap of
                                Just eid -> (fst s, snd s <> [(row, eid)])
                                Nothing -> (fst s <> [row], snd s)) ([],[]) earnedVacations

      planmillNameToPersonio name =
          case T.splitOn ", " name of
            [surname, firstname] -> firstname <> " " <> surname
            _ -> name

      personioNameMap :: Map Text P.EmployeeId
      personioNameMap = Map.fromList $ (\e -> (e ^. P.employeeFullname, e ^. P.employeeId)) <$> pemployees

renderReportSingle :: P.EmployeeId -> Integer -> PM.EarnedVacations -> [P.Employee] -> HtmlPage "vacation-report-single"
renderReportSingle eid currentYear earnedVacations pemployees = page_ "German vacation report" (Just NavVacationReport) $ do
    case Map.lookup eid toHoliday of
      Just h ->  pre_ $ toHtml $ template h
      Nothing -> mempty
  where
      toHoliday = Map.fromListWith (\h1 h2 -> PersonHoliday (employeeId h1) (employeeFirstName h1) (holidays h1 <> holidays h2))
          $ (\h -> (employeeId h, h)) <$> (catMaybes $ rowToHoliday <$> V.toList earnedVacations)

      planmillNameToPersonio name =
          case T.splitOn ", " name of
            [surname, firstname] -> firstname <> " " <> surname
            _ -> name

      rowToHoliday row = (\(eid', firstName) ->
                            PersonHoliday
                            eid'
                            firstName
                            (Map.singleton (PM._vacationYear row) (Holiday (PM._vacationAnnualHoliday row) (PM._vacationUsedAnnualHoliday row))))
                         <$> Map.lookup (planmillNameToPersonio $ PM._vacationUserName row) personioNameMap

      personioNameMap :: Map Text (P.EmployeeId, Text)
      personioNameMap = Map.fromList $ (\e -> (e ^. P.employeeFullname, (e ^. P.employeeId, e ^. P.employeeFirst))) <$> pemployees

      template holiday = case Map.lookup (Just $ fromInteger currentYear) (holidays holiday) of
        Just hds -> renderTemplate (employeeFirstName holiday) (annualHoliday hds) (usedAnnualHoliday hds) (sum $ (\x -> annualHoliday x - usedAnnualHoliday x) <$> Map.elems (holidays holiday)) (fromInteger currentYear)
        Nothing -> mempty

renderTemplate :: Text -> Double -> Double -> Double -> Int -> LazyText
renderTemplate firstName annualHoliday' usedAnnualHoliday' allRemainingAnnualHoliday currentYear= renderMustache earnedVacationsTemplate $ object
    [ "firstName"                  .= firstName
    , "annualHoliday"              .= annualHoliday'
    , "usedAnnualHoliday"          .= usedAnnualHoliday'
    , "allRemainingAnnualHoliday"  .= allRemainingAnnualHoliday
    , "currentYear"                .= currentYear
    ]

earnedVacationsTemplate :: Template
earnedVacationsTemplate = either (error . show) id
    $ compileMustacheText "earned-vacations.template"
    $(makeRelativeToProject "earned-vacations.template" >>= embedStringFile)
{-# NOINLINE earnedVacationsTemplate #-}
