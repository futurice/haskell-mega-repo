{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Personio (personioPage) where

import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API               (safeLink)

import Futurice.App.Checklist.API    (checklistApi, createEmployeePageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Personio

personioPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> UTCTime     -- ^ now
    -> [Personio.Employee]
    -> HtmlPage "personio"
personioPage _world authUser now employees0 = checklistPage_ "Import from personio" authUser $ do
    -- Title
    header "Import from Personio" []

    fullRow_ $ div_ [ class_ "callout" ] $ ul_ $
        li_ "Shows people who start in the next 90 days"

    -- Table
    fullRow_ $ table_ $ do
        thead_ $ tr_ $ do
            th_ "Personio ID"
            th_ "Name"
            th_ "Exists"
            th_ "Login"
            th_ "Tribe"
            th_ "Office"
            th_ "Internal"
            th_ "Hire date"
            th_ "Create"

        tbody_ $ for_ employees $ \e -> tr_ $ do
            td_ $ toHtml $ e ^. Personio.employeeId
            td_ $ toHtml $ (e ^. Personio.employeeFirst) <> " " <> (e ^. Personio.employeeLast)
            td_ $ "not implemented"
                {-
                boolHtml $ any
                (== e ^. Personio.employeeLogin)
                $ world ^.. worldEmployees . folded .  employeeFUMLogin
                -}
            td_ $ traverse_ toHtml $ e ^. Personio.employeeLogin
            td_ $ toHtml $ e ^. Personio.employeeTribe
            td_ $ toHtml $ e ^. Personio.employeeOffice
            td_ $ traverse_ toHtml $ e ^. Personio.employeeEmploymentType
            td_ $ traverse_ (toHtml . show) $ e ^. Personio.employeeHireDate
            td_ $ button_
                [ class_ "button"
                , data_ "futu-link-button" $ linkToText
                $ safeLink checklistApi createEmployeePageEndpoint Nothing (e ^? Personio.employeeId)
                ]
                "Import"

  where
    today = utctDay now
    hday = addDays 90 today

    employees = employees0
        & filter predicate
        & sortOn (Down . view Personio.employeeHireDate)

    predicate e = case e ^. Personio.employeeHireDate of
        Nothing -> False
        Just d  -> today <= d && d < hday

{-
    boolHtml :: Monad m => Bool -> HtmlT m ()
    boolHtml True = "YES"
    boolHtml False = "NO"
-}
