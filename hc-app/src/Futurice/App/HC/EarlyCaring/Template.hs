{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.EarlyCaring.Template (renderTemplate) where

import Data.Aeson                (object, (.=))
import Data.FileEmbed            (embedStringFile, makeRelativeToProject)
import Data.Fixed                (Deci)
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month       (Month, dayToMonth)
import Numeric.Interval.NonEmpty (Interval, inf)
import Prelude ()
import Text.Microstache          (Template, compileMustacheText, renderMustache)

import qualified Data.Text as T
import qualified Personio  as P

import Futurice.App.HC.EarlyCaring.Types

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
    month1 = dayToMonth $ inf interval
    month2 = succ month1

    flex = mapMaybe mkBalance balances
    mkBalance b
        | balanceNormalFlex today b = Nothing
        | otherwise = Just $ object
            [ "name"    .= (balanceEmployee b ^. P.employeeFullname)
            , "balance" .= formatHours (balanceHours b)
            , "missing" .= formatHours (balanceMissingHours b)
            , "sum"     .= formatHours (balanceHours b + balanceMissingHours b)
            , "month1"  .= monthToObject b month1
            , "month2"  .= monthToObject b month2
            ]

    monthToObject :: Balance -> Month -> Value
    monthToObject b m  = case balanceMonthFlex b ^. ix m of
        MonthFlex marked capa -> object
            [ "month"  .= m
            , "diff"   .= formatHours (marked - capa)
            , "marked" .= formatHours' marked
            , "capa"   .= formatHours' capa
            ]

    sickCount = mapMaybe mkCount balances
    mkCount b
        | balanceNormalAbsences today b = Nothing
        | otherwise = Just $ object
            [ "name"    .= T.justifyLeft 25 '.' (balanceEmployee b ^. P.employeeFullname)
            , "cases"   .= countAbsences today (balanceAbsences b)
            ]

    sickDays = mapMaybe mkDays balances
    mkDays b
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

earlyCaringTemplate :: Template
earlyCaringTemplate = either (error . show) id
    $ compileMustacheText "early-caring.template"
    $(makeRelativeToProject "early-caring.template" >>= embedStringFile)
{-# NOINLINE earlyCaringTemplate #-}
