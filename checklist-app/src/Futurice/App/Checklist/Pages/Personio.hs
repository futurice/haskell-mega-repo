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
personioPage world authUser now employees0 = checklistPage_ "Import from Personio" [] authUser (Just NavPersonio) $ do
    -- navigation buttons
    div_ [ class_ "button-group" ] $ for_ combinations $ \(what, when') -> do
        a_ [ class_ "button", href_ $ "#" <> anchor what when' ] $
            title what when'

    -- info
    fullRow_ $ div_ [ class_ "callout" ] $ ul_ $ do
        li_ "Checklist is shown when' employee Personio id or FUM login matches"
        li_ $ em_ "In the past" <> " lists show people left in last 30 days"


    -- Tables
    for_ combinations $ \(what, when') -> do
        a_ [ name_ $ anchor what when', href_ $ "#" <> anchor what when' ] $
            subheader_ $ title what when'
        employeeTable True world $ filteredEmployees what when'

  where
    today = utctDay now
    loday = addDays (-30) today

    combinations = [ (what, when') | what <- [ Starting, Leaving], when' <- [ Future, Past] ]

    anchor what when' = what' <> "-" <> when'' where
        what' = case what of
            Starting -> "starting"
            Leaving  -> "leaving"
        when'' = case when' of
            Future -> "future"
            Past   -> "past"

    title Starting Future = "Starting in the future"
    title Starting Past   = "Started in the past"
    title Leaving  Future = "Leaving in the future"
    title Leaving  Past   = "Left in the past"

    filteredEmployees what when' = employees0
        & filter predicate
        & sortOn (Down . view dateLens)
      where
        predicate e = case e ^. dateLens of
            Nothing -> False
            Just d  -> case when' of
                Future -> today <= d
                Past   -> loday <= d && d < today

        dateLens = case what of
            Starting -> Personio.employeeHireDate
            Leaving  -> Personio.employeeEndDate

employeeTable :: Monad m => Bool -> World -> [Personio.Employee] -> HtmlT m ()
employeeTable hire world employees = fullRow_ $ sortableTable_ $ do
    thead_ $ tr_ $ do
        th_ "Personio ID"
        th_ "Name"
        th_ "Checklists"
        th_ "Login"
        th_ "Tribe"
        th_ "Office"
        th_ "Internal"
        th_ $ if hire then "Hire date" else "End Date"
        th_ "Job offer accepted"
        th_ "Create"

    tbody_ $ for_ employees $ \e -> tr_ $ do
        td_ $ toHtml $ e ^. Personio.employeeId
        td_ $ toHtml $ (e ^. Personio.employeeFirst) <> " " <> (e ^. Personio.employeeLast)
        td_ $ forWith_ ", " (matchingEmployees e) $ \e' ->
            a_ [ employeePageHref e' ] $ maybe "?" (view nameHtml) $
                world ^? worldLists . ix (e' ^. employeeChecklist)
        td_ $ traverse_ toHtml $ e ^. Personio.employeeLogin
        td_ $ toHtml $ e ^. Personio.employeeTribe
        td_ $ toHtml $ e ^. Personio.employeeOffice
        td_ $ traverse_ toHtml $ e ^. Personio.employeeEmploymentType
        td_ $ traverse_ (toHtml . show) $ e ^. if hire then Personio.employeeHireDate else Personio.employeeEndDate
        td_ $ traverse_ (toHtml . show) $ e ^. Personio.employeeJobOfferAccepted
        td_ $ a_
            [ class_ "button"
            , href_ $ linkToText
            $ safeLink checklistApi createEmployeePageEndpoint Nothing (e ^? Personio.employeeId) $ not hire
            ]
            "Import"
  where
    matchingEmployees e = filter (predicate' e) $ world ^.. worldEmployees . folded

    predicate' e e'
        | Just x <- e' ^. employeePersonio = x == e ^. Personio.employeeId
        | Just x <- e' ^. employeeFUMLogin = Just x == e ^. Personio.employeeLogin
        | otherwise = False

data What = Starting | Leaving
data When = Future | Past
