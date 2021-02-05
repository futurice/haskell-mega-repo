{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.EarlyCaring.Template (renderTemplate, emptyTemplate, renderSummaryTemplate) where

import Data.Aeson                (object, (.=))
import Data.Fixed                (Deci)
import Futurice.Office           (Office, officeToText)
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month       (dayToMonth)
import Numeric.Interval.NonEmpty (Interval, inf)
import Prelude ()
import Text.Microstache          (Template, compileMustacheText, renderMustache)

import qualified Data.Map       as Map
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import qualified Personio       as P

import Futurice.App.HC.EarlyCaring.Types

-- | Will the template be empty based on template rules
emptyTemplate :: Day -> Interval Day -> [Balance] -> Bool
emptyTemplate today interval balances = null flex && null sickCount && null sickDays
  where
    flex = mapMaybe (mkBalance today interval) balances
    sickCount = mapMaybe (mkCount today) balances
    sickDays = mapMaybe (mkDays today) balances

renderTemplate
    :: Text      -- ^ supervisor name
    -> Day       -- ^ today
    -> Interval Day
    -> [Balance]
    -> LazyText
renderTemplate name today interval balances = renderMustache earlyCaringTemplate $ object
    [ "name"        .= name
    , "today"       .= pred today
    , "balances"    .= flex
    , "hasBalances" .= not (null flex)
    , "sickCount"   .= sickCount
    , "sickDays"    .= sickDays
    , "hasSick"     .= not (null sickCount && null sickDays)
    ]
  where
    flex = mapMaybe (mkBalance today interval) balances
    sickCount = mapMaybe (mkCount today) balances
    sickDays = mapMaybe (mkDays today) balances

monthToObject :: Balance -> Month -> Value
monthToObject b m  = case balanceMonthFlex b ^. ix m of
  MonthFlex marked capa -> object
      [ "month"  .= m
      , "diff"   .= formatHours (marked - capa)
      , "marked" .= formatHours' marked
      , "capa"   .= formatHours' capa
      ]

mkCount :: Day -> Balance -> Maybe Value
mkCount today b
    | balanceNormalAbsences today b = Nothing
    | otherwise = Just $ object
      [ "name"    .= T.justifyLeft 25 '.' (balanceEmployee b ^. P.employeeFullname)
      , "cases"   .= countAbsences today (balanceAbsences b)
      ]

mkBalance :: Day -> Interval Day -> Balance -> Maybe Value
mkBalance today interval b
    | balanceNormalFlex today b = Nothing
    | otherwise = Just $ object
      [ "name"    .= (balanceEmployee b ^. P.employeeFullname)
      , "balance" .= formatHours (balanceHours b)
      , "missing" .= formatHours (balanceMissingHours b)
      , "sum"     .= formatHours (balanceHours b + balanceMissingHours b)
      , "month1"  .= monthToObject b month1
      , "month2"  .= monthToObject b month2
      ]
  where
    month1 = dayToMonth $ inf interval
    month2 = succ month1

mkDays :: Day -> Balance -> Maybe Value
mkDays today b
    | balanceNormalAbsenceDays today b = Nothing
    | otherwise = Just $ object
      [ "name"    .= T.justifyLeft 25 '.' (balanceEmployee b ^. P.employeeFullname)
      , "days"    .= countAbsenceDays today (balanceAbsences b)
      ]

formatHours :: NDT 'Hours Deci -> Text
formatHours n = sign <> formatHours' n where
    sign | n > 0 = "+"
         | otherwise = ""

formatHours' :: NDT 'Hours Deci -> Text
formatHours' n = textShow (unNDT n)

renderSummaryTemplate
    :: [Balance]      -- ^ balances
    -> Day            -- ^ today
    -> [P.Employee]   -- ^ supervisors with email problems
    -> LazyText
renderSummaryTemplate balances today supervisors = LT.replace "\n" "" $ renderMustache earlyCaringSummaryTemplate $ object
    [ "timestamp"        .= today
    , "balanceMap"       .= (mkOfficeMap <$> sortOn fst (Map.toList $ Map.fromListWith (<>) $ mapMaybe toOffice $ mapMaybe (extractBalances today) balances))
    , "sicknessMap"      .= (mkOfficeMap <$> sortOn fst (Map.toList $ Map.fromListWith (<>) $ mapMaybe toOffice $ mapMaybe (extractSick today) balances))
    , "missingEmails"    .= ((^. P.employeeFullname) <$> supervisors)
    , "hasMissingEmails" .= not (null supervisors)
    ]
  where
    toOffice b =
        case balanceSupervisor b of
          Just s -> Just (balanceEmployee b ^. P.employeeOffice, [(s, balanceEmployee b)])
          Nothing -> Nothing

extractBalances :: Day -> Balance -> Maybe Balance
extractBalances today b
    | balanceNormalFlex today b = Nothing
    | otherwise = Just b

extractSick ::  Day -> Balance -> Maybe Balance
extractSick today b
    | balanceNormalAbsenceDays today b = Nothing
    | otherwise = Just b

mkEmployee :: (P.Employee, P.Employee) -> Value
mkEmployee (supervisor, e) = object
    [ "name"       .= (e ^. P.employeeFullname)
    , "supervisor" .= (supervisor ^. P.employeeFullname)
    ]

mkOfficeMap :: (Office, [(P.Employee, P.Employee)]) -> Value
mkOfficeMap (office, emps) = object
    [ "office"    .= officeToText office
    , "employees" .= fmap mkEmployee emps
    ]

earlyCaringTemplate :: Template
earlyCaringTemplate = either (error . show) id
    $ compileMustacheText "early-caring.template"
    $(makeRelativeToProject "early-caring.template" >>= embedStringFile)
{-# NOINLINE earlyCaringTemplate #-}

earlyCaringSummaryTemplate :: Template
earlyCaringSummaryTemplate = either (error . show) id
    $ compileMustacheText "early-caring-summary.template"
    $(makeRelativeToProject "early-caring-summary.template" >>= embedStringFile)
{-# NOINLINE earlyCaringSummaryTemplate #-}
