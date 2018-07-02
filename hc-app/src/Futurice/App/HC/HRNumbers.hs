{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.HRNumbers (hrnumbers) where

import Futurice.Prelude
import Prelude ()
import Data.Maybe (isJust)

import qualified Personio as P

import Futurice.App.HC.Markup

hrnumbers :: [P.Employee] -> Day -> HtmlPage "hr-numbers"
hrnumbers es' today = page_ "HR Numbers" Nothing $ do
    -- active internals
    let predicate x = and 
            [ P.employeeIsActive today x
            , x ^. P.employeeEmploymentType == Just P.Internal
            , isJust (x ^. P.employeeHRNumber)
            , x ^. P.employeeHRNumber /= Just 0 -- shouldn't be necessary soon
            ]

    let es = filter predicate es'

    fullRow_ $ sortableTable_ $ do
        thead_ $ do
            th_ "Personio"
            th_ "Name"
            th_ "Office"
            th_ "HR Number"

        tbody_ $ for_ (sortOn (view P.employeeLast) es) $ \e -> tr_ $ do
            td_ $ toHtml $ e ^. P.employeeId
            td_ $ toHtml $ e ^. P.employeeFullname
            td_ $ toHtml $ e ^. P.employeeOffice
            td_ $ traverse_ (toHtml . show) $ e ^. P.employeeHRNumber
