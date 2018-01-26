{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.HC.PrivateContacts (privateContacts) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

privateContacts :: [P.Employee] -> HtmlPage "private-contacts"
privateContacts es = page_ "Personio private contacts" $ do
    fullRow_ $ h1_ "Personio private contacts"

    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ "id"
            th_ "name"
            th_ "fum"
            th_ "status"
            th_ "hire-date"
            th_ "end-date"
            th_ "internal"
            th_ "type"
            th_ "private email"
            th_ "private phone"

        tbody_ $ for_ (sortOn (view P.employeeFullname) es) $ \e -> tr_ $ do
            td_ $ toHtml $ e ^. P.employeeId
            td_ $ toHtml $ e ^. P.employeeFullname
            td_ $ traverse_ toHtml $ e ^. P.employeeLogin
            td_ $ toHtml $ e ^. P.employeeStatus
            td_ $ toHtml $ maybe "-" show $ e ^. P.employeeHireDate
            td_ $ toHtml $ maybe "-" show $ e ^. P.employeeEndDate
            td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeEmploymentType
            td_ $ toHtml $ maybe "Unknown" show $ e ^. P.employeeContractType
            td_ $ traverse_ toHtml $ e ^. P.employeeHomeEmail
            td_ $ traverse_ toHtml $ e ^. P.employeeHomePhone
