{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.VacationReport where

import Data.Aeson                         (object, (.=))
import Data.Time                          (fromGregorian, toGregorian)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Futurice.Prelude
import Prelude ()
import Servant.API                        (ToHttpApiData (..))
import Servant.Links                      (Link, fieldLink)
import Text.Microstache
       (Template, compileMustacheText, renderMustache)

import Futurice.App.HC.API
import Futurice.App.HC.Markup

import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified FUM.Types.Login as FUM
import qualified Personio        as P
import qualified PlanMill        as PM

data Holiday = Holiday
    { annualHoliday     :: !Double
    , usedAnnualHoliday :: !Double
    , holidayYear       :: !(Maybe Int)
    , expirationDate    :: !(Maybe Day)
    } deriving (Show)

data PersonHoliday = PersonHoliday
    { employeeId        :: !P.EmployeeId
    , employeeFirstName :: !Text
    , holidays          :: !(Map (Maybe Int) Holiday)
    } deriving (Show)

data RenderTemplate = RenderTemplate
    { rtFirstName                 :: !Text
    , rtHolidays                  :: ![Holiday]
    , rtAllRemainingAnnualHoliday :: !Double
    }

data VacationReportLog = VacationReportLog
    { vrSendTime   :: !UTCTime
    , vrReceiver   :: !P.EmployeeId
    , vrSender     :: !FUM.Login
    } deriving (FromRow, ToRow, Generic)

renderReport :: Map P.EmployeeId VacationReportLog -> PM.EarnedVacations -> [P.Employee] -> HtmlPage "vacation-report"
renderReport logs earnedVacations pemployees = page_ "German vacation report" (Just NavVacationReport) $ do
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
    sendAllButton
    sortableTable_ $ do
        thead_ $ do
            th_ "Ignore"
            th_ "Name"
            th_ "Last notification time"
            th_ "Year"
            th_ "Annual holiday"
            th_ "Used annual holiday"
            th_ "Annual holidays remaining"
            th_ "Bonus holiday"
            th_ "Bonus holiday remaining"
            th_ "Other annual holiday"
            th_ "Saved leaves"
            th_ "Saved leaves remaining"
            th_ "Office"
            th_ ""
        tbody_ $ do
            for_ (snd vacationPart) $ \(row,emp) -> when (shouldBeShown emp) $ tr_ $ do
                td_ $ checkbox_ False [ data_ "futu-vacation-report-ignore-user" $ textShow $ employeeNumber $ emp ^. P.employeeId]
                td_ $ toHtml $ PM._vacationUserName row
                td_ $ toHtml $ maybe "" textShow $ vrSendTime <$> logs ^.at (emp ^. P.employeeId)
                td_ $ toHtml $ maybe "" textShow $ PM._vacationYear row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationUsedAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationAnnualHolidaysRemaining row
                td_ $ toHtml $ textShow $ PM._vacationBonusHoliday row
                td_ $ toHtml $ textShow $ PM._vacationBonusHolidayRemaining row
                td_ $ toHtml $ textShow $ PM._vacationOtherAnnualHoliday row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeave row
                td_ $ toHtml $ textShow $ PM._vacationSavedLeaveRemaining row
                td_ $ toHtml $ emp ^. P.employeeOffice
                td_ $ a_ [ style_ "display: block", recordHref_ recVacationReportEmail $ emp ^. P.employeeId] $ toHtml ("Show email" :: Text)

    sendAllButton
  where
      employeeNumber (P.EmployeeId eid) = eid
      shouldBeShown e = e ^. P.employeeExpat == False && e ^. P.employeeStatus /= P.Inactive && e ^. P.employeeEmploymentType == Just P.Internal
      vacationPart = foldl (\s row ->
                              case Map.lookup (planmillNameToPersonio $ PM._vacationUserName row) personioNameMap of
                                Just eid -> (fst s, snd s <> [(row, eid)])
                                Nothing -> (fst s <> [row], snd s)) ([],[]) earnedVacations

      planmillNameToPersonio name =
          case T.splitOn ", " name of
            [surname, firstname] -> firstname <> " " <> surname
            _ -> name

      personioNameMap :: Map Text P.Employee
      personioNameMap = Map.fromList $ (\e -> (e ^. P.employeeFullname, e)) <$> pemployees

      -- sendAllButton = button_ [class_ "button primary", disabled_ "disabled"] "Send to all"
      sendAllButton = button_ [ id_ "futu-vacation-report-send-all"
                              , class_ "button primary"
                              , disabled_ "disabled"
                              , data_ "futu-vacation-report-send-all" $ linkToText $ fieldLink recVacationReportSubmit] "Send to all"

personHolidays :: P.Employee -> PM.EarnedVacations -> Maybe PersonHoliday
personHolidays emp earnedVacations = Map.lookup (emp ^. P.employeeId) toHoliday
  where
    rowToHoliday row = PersonHoliday
                       (emp ^. P.employeeId)
                       (emp ^. P.employeeFirst)
                       (Map.singleton
                         (PM._vacationYear row)
                         (Holiday (PM._vacationAnnualHoliday row) (PM._vacationUsedAnnualHoliday row) (PM._vacationYear row) (expirationDate' <$> PM._vacationYear row)))

    toHoliday = Map.fromListWith (\h1 h2 -> PersonHoliday (employeeId h1) (employeeFirstName h1) (holidays h1 <> holidays h2))
        $ (\h -> (employeeId h, h)) <$> (rowToHoliday <$> filter (\v -> PM._vacationUserName v == personioToPlanmillname) (V.toList earnedVacations))

    personioToPlanmillname = emp ^. P.employeeLast  <> ", " <> emp ^. P.employeeFirst

    expirationDate' 2019 = $(mkDay "2020-12-31")
    expirationDate' year = fromGregorian (toInteger year + 1) 3 31

renderReportSingle :: P.Employee -> Integer -> PM.EarnedVacations -> HtmlPage "vacation-report-single"
renderReportSingle employee currentYear earnedVacations =
    page_ "German vacation report" (Just NavVacationReport) $ pre_ $ toHtml $ fromMaybe mempty $ reportSingle employee currentYear earnedVacations

hasHolidaysInCurrentYear :: Integer -> PersonHoliday -> Bool
hasHolidaysInCurrentYear currentYear hs = Map.member (Just $ fromInteger currentYear) (holidays hs)

reportSingle :: P.Employee -> Integer -> PM.EarnedVacations -> Maybe LazyText
reportSingle employee currentYear earnedVacations = template <$> do
    hol <- personHolidays employee earnedVacations
    if hasHolidaysInCurrentYear currentYear hol then Just hol else Nothing
  where
      relevantHolidays holiday =
          (Map.elems $ Map.filterWithKey (\k _ -> k == Just (fromInteger currentYear) || k == Just (fromInteger $ currentYear - 1)) $ holidays holiday)
      zeroFloor n | n < 0 = 0
                  | otherwise = n
      template holiday =
          renderTemplate $ RenderTemplate
          { rtFirstName                 = (employeeFirstName holiday)
          , rtHolidays                  = relevantHolidays holiday
          , rtAllRemainingAnnualHoliday = (sum $ (\x -> zeroFloor(annualHoliday x - usedAnnualHoliday x)) <$> relevantHolidays holiday)
          }

renderTemplate :: RenderTemplate -> LazyText
renderTemplate (RenderTemplate firstName holidays' allRemainingAnnualHoliday) =
    renderMustache earnedVacationsTemplate $ object
    [ "firstName"                  .= firstName
    , "holidays"                   .= map mkHoliday holidays'
    , "allRemainingAnnualHoliday"  .= allRemainingAnnualHoliday
    ]
  where
    mkHoliday (Holiday annualHoliday' usedAnnualHoliday' holidayYear' expirationDate') = object
        [ "annualHoliday"     .= annualHoliday'
        , "usedAnnualHoliday" .= usedAnnualHoliday'
        , "holidayYear"       .= holidayYear'
        , "expirationDate"    .= (toDateString <$> expirationDate')
        , "remainingDays"     .= (annualHoliday' - usedAnnualHoliday')
        ]
    toDateString date = let (y, m, d) = toGregorian date in show d <> "." <> show m <> "." <> show y


linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

earnedVacationsTemplate :: Template
earnedVacationsTemplate = either (error . show) id
    $ compileMustacheText "earned-vacations.template"
    $(makeRelativeToProject "earned-vacations.template" >>= embedStringFile)
{-# NOINLINE earnedVacationsTemplate #-}
