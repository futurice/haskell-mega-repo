{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PersonioProxy.StatsPage (statsPage) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation (page_)
import Futurice.Office
import Futurice.Prelude
import Prelude ()

import Futurice.App.PersonioProxy.Markup

import qualified Data.Map as Map
import qualified Personio as P

statsPage :: [P.Employee] -> HtmlPage "stats"
statsPage emps = page_ "Stats page" (Just NavStats) $ do
    fullRow_ $ do
        table_ $ do
            thead_ $ do
                th_ ""
                th_ "Internal"
                th_ "External"
                th_ "="
            tbody_ $ do
                for_ (sortOn (officeToText . fst) $ Map.toList officeMap) $ \(office, emps') -> do
                    tr_ $ do
                        td_ $ toHtml $ officeToText office
                        td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps'
                        td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeEmploymentType == Just P.External) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps'
                        td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps'
                tr_ $ do
                    td_ $ b_ "Sum"
                    td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps
                    td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeEmploymentType == Just P.External) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps
                    td_ $ toHtml $ textShow $ length $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emps
  where
    officeMap :: Map Office [P.Employee]
    officeMap = Map.fromListWith (<>) $ (\emp -> (emp ^. P.employeeOffice, [emp])) <$> emps
