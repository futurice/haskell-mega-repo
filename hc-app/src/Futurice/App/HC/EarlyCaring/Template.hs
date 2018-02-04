{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.EarlyCaring.Template (renderTemplate) where

import Data.Aeson       (object, (.=))
import Data.FileEmbed   (embedStringFile, makeRelativeToProject)
import Futurice.Prelude
import Futurice.Time
import Data.Fixed (Deci)
import Prelude ()
import Text.Microstache (Template, compileMustacheText, renderMustache)

import qualified Data.Text as T
import qualified Personio  as P

import Futurice.App.HC.EarlyCaring.Types

renderTemplate
    :: Text      -- ^ supervisor name
    -> Day       -- ^ today
    -> [Balance]
    -> LazyText
renderTemplate name today balances = renderMustache earlyCaringTemplate $ object
    [ "name"        .= name
    , "today"       .= today
    , "balances"    .= flex
    , "hasBalances" .= not (null flex)
    , "sickCount"   .= sickCount
    , "sickDays"    .= sickDays
    , "hasSick"     .= not (null sickCount && null sickDays)
    ]
  where
    flex = mapMaybe mkBalance balances
    mkBalance b
        | balanceNormalFlex today b = Nothing
        | otherwise = Just $ object
            [ "name"    .= T.justifyLeft 25 '.' (balanceEmployee b ^. P.employeeFullname)
            , "balance" .= formatHours (balanceHours b)
            , "missing" .= formatHours (balanceMissingHours b)
            , "sum"     .= formatHours (balanceHours b + balanceMissingHours b)
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
formatHours n = T.justifyRight 7 ' ' $ sign <> textShow (unNDT n) where
    sign | n > 0 = "+"
         | otherwise = ""

earlyCaringTemplate :: Template
earlyCaringTemplate = either (error . show) id
    $ compileMustacheText "early-caring.template"
    $(makeRelativeToProject "early-caring.template" >>= embedStringFile)
{-# NOINLINE earlyCaringTemplate #-}
