{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Personio (personioPage) where

import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API               (safeLink)

import Futurice.App.Checklist.API
       (checklistApi, createEmployeePageEndpoint, personioPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Personio

personioPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> UTCTime     -- ^ now
    -> [Personio.Employee]
    -> Bool
    -> Bool
    -> HtmlPage "personio"
personioPage world authUser now employees0 startingDescOrder leavingDescOrder = checklistPage_ "Import from personio" authUser $ do
    -- Title
    header "Import from Personio" []

    fullRow_ $ div_ [ class_ "callout" ] $ ul_ $ do
        li_ "Shows people who start in the next 90 days"
        li_ "Checklist shown when employee personio id or fum login matches"

    -- Table
    subheader_ "Starting"
    employeeTable True world startingEmployees startingDescOrder leavingDescOrder

    -- Table
    subheader_ "Leaving"
    employeeTable False world leavingEmployees startingDescOrder leavingDescOrder

  where
    today = utctDay now
    hday = addDays 90 today

    startingEmployees = employees0
        & filter predicate
        & (if startingDescOrder then sortOn (Down . view Personio.employeeHireDate) else sortOn (view Personio.employeeHireDate))
      where
        predicate e = case e ^. Personio.employeeHireDate of
            Nothing -> False
            Just d  -> today <= d && d < hday

    leavingEmployees = employees0
        & filter predicate
        & (if leavingDescOrder then sortOn (Down . view Personio.employeeEndDate) else sortOn (view Personio.employeeEndDate))
      where
        predicate e = case e ^. Personio.employeeEndDate of
            Nothing -> False
            Just d  -> today <= d

employeeTable :: Monad m => Bool -> World -> [Personio.Employee] -> Bool -> Bool -> HtmlT m ()
employeeTable hire world employees startingDescOrder leavingDescOrder = fullRow_ $ table_ $ do
    thead_ $ tr_ $ do
        th_ "Personio ID"
        th_ "Name"
        th_ "Checklists"
        th_ "Login"
        th_ "Tribe"
        th_ "Office"
        th_ "Internal"
        th_ $ if hire then
                a_ [href_ $ linkToText $ safeLink checklistApi personioPageEndpoint (not startingDescOrder) leavingDescOrder ] "Hire date"
              else
                a_ [href_ $ linkToText $ safeLink checklistApi personioPageEndpoint startingDescOrder (not leavingDescOrder) ] "End Date"
        th_ "Create"

    tbody_ $ for_ employees $ \e -> tr_ $ do
        td_ $ toHtml $ e ^. Personio.employeeId
        td_ $ toHtml $ (e ^. Personio.employeeFirst) <> " " <> (e ^. Personio.employeeLast)
        td_ $ for_ (matchingEmployees e) $ \e' ->
            a_ [ employeePageHref e' ] $ maybe "?" (view nameHtml) $
                world ^? worldLists . ix (e' ^. employeeChecklist)
        td_ $ traverse_ toHtml $ e ^. Personio.employeeLogin
        td_ $ toHtml $ e ^. Personio.employeeTribe
        td_ $ toHtml $ e ^. Personio.employeeOffice
        td_ $ traverse_ toHtml $ e ^. Personio.employeeEmploymentType
        td_ $ traverse_ (toHtml . show) $ e ^. if hire then Personio.employeeHireDate else Personio.employeeEndDate
        td_ $ button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi createEmployeePageEndpoint Nothing (e ^? Personio.employeeId) $ not hire
            ]
            "Import"
  where
    matchingEmployees e = filter (predicate' e) $ world ^.. worldEmployees . folded

    predicate' e e'
        | Just x <- e' ^. employeePersonio = x == e ^. Personio.employeeId
        | Just x <- e' ^. employeeFUMLogin = Just x == e ^. Personio.employeeLogin
        | otherwise = False
