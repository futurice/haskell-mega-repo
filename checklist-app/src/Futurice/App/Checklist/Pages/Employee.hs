{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (forOf_, re)
import Data.Aeson                (ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Set.Lens             (setOf)
import Futurice.Lucid.Foundation
import Servant.API               (safeLink)
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.API
       (checklistApi, checklistPageEndpoint, createEmployeePageEndpoint,
       employeeAuditPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Personio as P

-- |
--
-- === Preconditions
--
-- * 'Employee' is in the 'World'.
employeePage
    :: World
    -> AuthUser
    -> Employee
    -> [P.Employee]
    -> HtmlPage "employee"
employeePage world authUser employee _personios = checklistPage_ (view nameText employee) authUser $ do
    -- Title
    header (employee ^. nameText) []

    -- Buttons
    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        for_ (employee ^. employeePersonio) $ \pid ->
            toHtml pid `with` [ class_ " button hollow" ]
        for_ mlist $ \cl -> button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi checklistPageEndpoint (cl ^. identifier)
            ]
            $ toHtml $ "Checklist: " <> cl ^. nameText
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi employeeAuditPageEndpoint $ employee ^. identifier
            ]
            "Audit log"
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi createEmployeePageEndpoint (employee ^? identifier) Nothing
            ]
            "Create employee sing this employee as a template"
        button_
            [ class_ "button alert"
            , futuId_ "employee-archive"
            , data_ "futu-employee-id" $ employee ^. identifierText
            ]
            "Archive employee"

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "employee-edit", data_ "futu-employee-id" $ employee ^. identifierText ] $ do
        row_ $ large_ 12 $ label_ $ do
            "First name"
            input_ [ futuId_ "employee-firstname", type_ "text", value_ $ employee ^. employeeFirstName ]
        row_ $ large_ 12 $ label_ $ do
            "Last name"
            input_ [ futuId_ "employee-lastname", type_ "text", value_ $ employee ^. employeeLastName ]
        row_ $ large_ 12 $ label_ $ do
            "Contract"
            select_ [ futuId_ "employee-contract-type" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeContractType)
                    [ value_ $ x ^. re _ContractType ]
                    $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Office"
            select_ [ futuId_ "employee-location" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeOffice)
                    [ value_ $ x ^. re _Office ]
                    $ toHtml $ x ^. re _Office
        row_ $ large_ 12 $ label_ $ do
            "Confirmed"
            br_ []
            checkbox_ (employee ^. employeeConfirmed) [ futuId_ "employee-confirmed" ]
        row_ $ large_ 12 $ label_ $ do
            "Due day"
            input_ [ futuId_ "employee-starting-day", type_ "date", value_ $ toQueryParam $ employee ^. employeeStartingDay  ]
        row_ $ large_ 12 $ label_ $ do
            "Supervisor"
            input_ [ futuId_ "employee-supervisor", type_ "text", value_ $ toQueryParam $ employee ^. employeeSupervisor, data_ "futu-values" $ encodeToText supervisors ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe"
            select_ [ futuId_ "employee-tribe", type_ "text" ] $ do
                for_ [ minBound .. maxBound ] $ \tribe ->
                    optionSelected_ (tribe == employee ^. employeeTribe)
                        [ value_ $ toQueryParam tribe ]
                        $ toHtml tribe
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info", rows_ "5" ] $ toHtml $ employee ^. employeeInfo
        row_ $ large_ 12 $ label_ $ do
            "Phone"
            -- TODO: maybe it's simpler to just define empty value
            input_ $ [ futuId_ "employee-phone", type_ "tel" ] ++
                catMaybes [ value_ <$> employee ^. employeePhone ]
        row_ $ large_ 12 $ label_ $ do
            "Private email"
            input_ $ [ futuId_ "employee-contact-email", type_ "email" ] ++
                catMaybes [ value_ <$> employee ^. employeeContactEmail ]
        row_ $ large_ 12 $ label_ $ do
            "FUM handle"
            input_ $ [ futuId_ "employee-fum-login", type_ "text" ] ++
                catMaybes [ value_ . toQueryParam <$> employee ^. employeeFUMLogin ]
        row_ $ large_ 12 $ label_ $ do
            "HR number"
            input_ $ [ futuId_ "employee-hr-number", type_ "text" ] ++
                catMaybes [ value_ . textShow <$> employee ^. employeeHRNumber ]
        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Save"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Tasks
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]  "Task"
            th_ [ title_ "Role" ]  "Role"
            th_ [ title_ "Check" ] "Check"
            th_ [ title_ "Comment" ] "Comment"
            th_ [ title_ "Who and when have done this task" ] "Audit"
        tbody_ $ forOf_ (worldTasksSorted (authUser ^. authUserTaskRole) . folded) world $ \task -> do
            let tid = task ^. identifier
            for_ (world ^? worldTaskItems . ix eid . ix tid) $ \taskItem -> tr_ $ do
                td_ $ taskLink task
                td_ $ roleHtml mlist (task ^. taskRole)
                td_ $ taskCheckbox_ world employee task
                td_ $ taskCommentInput_ world employee task
                td_ $ forOf_ _AnnTaskItemDone taskItem $ \(_, fumUser, timestamp) -> do
                    toHtml fumUser
                    " "
                    toHtml $ show $ localDay $ utcToHelsinkiTime timestamp

    when (authUser ^. _2 == TaskRoleIT) $ row_ $ large_ 12 $ do
        hr_ []
        button_
            [ class_ "button alert"
            , futuId_ "employee-remove"
            , data_ "futu-employee-id" $ employee ^. identifierText
            ] "DELETE EMPLOYEE"
  where
    eid = employee ^. identifier
    mlist = world ^? worldLists . ix (employee ^. employeeChecklist)

    supervisors :: [Text]
    supervisors = toList $ setOf (worldEmployees . folded . employeeSupervisor . getter toQueryParam) world

encodeToText :: ToJSON a => a -> Text
encodeToText = view strict . encodeToLazyText
