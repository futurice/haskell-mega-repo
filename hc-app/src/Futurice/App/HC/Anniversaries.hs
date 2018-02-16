{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.Anniversaries (anniversaries) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Data.Time (toGregorian, fromGregorian, addDays)
import Prelude ()

import qualified Personio as P

anniversaries :: [P.Employee] -> Day -> HtmlPage "anniversaries"
anniversaries es' today = page_ "Anniveraries" $ do
    -- active internals
    let es = filter (\x -> P.employeeIsActive today x && x ^. P.employeeEmploymentType == Just P.Internal) es'

    let maxDay = addDays 365 today
    let inrange day = today <= day && day <= maxDay
    let order day
            | (m, d) >= (m', d') = (m, d)
            | otherwise          = (m + 12, d)
          where
            (_, m, d) = toGregorian day
            (_, m', d') = toGregorian today

    fullRow_ $ h1_ "Anniversaries"

    fullRow_ $ div_ [ class_ "callout info" ] $ ul_ $ do
        li_ $ "Anniversaries in next 365 (a year) shown"
        li_ $ do
            "There are separate lists for "
            a_ [ href_ "#work" ] "Work anniversaries (10 years at futurice)"
            " and "
            a_ [ href_ "#birthday" ] "Birthdays"

    fullRow_ $ a_ [ name_ "work", href_ "#work"] $ h2_ "Work"
    fullRow_ $ table_ $ do
        thead_ $ do
            th_ "Personio"
            th_ "Name"
            th_ "Hire date"

        tbody_ $ for_ (sortOn (fmap order . view P.employeeHireDate) es) $ \e ->
            for_ (e ^. P.employeeHireDate) $ \hday -> do
                let (hyear, hmonth, hd) = toGregorian hday
                when (inrange $ fromGregorian (hyear + 10) hmonth hd) $ tr_ $ do
                    td_ $ toHtml $ e ^. P.employeeId
                    td_ $ toHtml $ e ^. P.employeeFullname
                    td_ $ toHtml $ show hday

    fullRow_ $ a_ [ name_ "birthday", href_ "#birthday"] $ h2_ "Birthdays"
    fullRow_ $ table_ $ do
        thead_ $ do
            th_ "Personio"
            th_ "Name"
            th_ "Birthday"
            th_ "Years"

        tbody_ $ for_ (sortOn (fmap order . view P.employeeBirthday) es) $ \e ->
            for_ (e ^. P.employeeBirthday) $ \bday -> do
                let (byear, bmonth, bd) = toGregorian bday
                    check y
                        | inrange (fromGregorian (byear + y) bmonth bd) =  Just y
                        | otherwise = Nothing
                for_ (check 20 <|> check 30 <|> check 40 <|> check 50 <|> check 60) $ \y -> tr_ $ do
                    td_ $ toHtml $ e ^. P.employeeId
                    td_ $ toHtml $ e ^. P.employeeFullname
                    td_ $ toHtml $ show bday
                    td_ $ toHtml $ show y
