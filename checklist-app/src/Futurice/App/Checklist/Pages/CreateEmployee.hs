{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.CreateEmployee (createEmployeePage) where

import Control.Lens              (forOf_, re)
import Data.Aeson                (ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Set.Lens             (setOf)
import FUM.Types.Login           (Login)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Personio

-- Template
data Tmpl = Tmpl
    { tmplPersonioId   :: !(Maybe Personio.EmployeeId)
    , tmplFirst        :: !Text
    , tmplLast         :: !Text
    , tmplContractType :: !(Maybe ContractType)
    , tmplOffice       :: !Office
    , tmplTribe        :: !Tribe
    , tmplStartingDay  :: !(Maybe Day)
    , tmplSupervisor   :: !Text
    , tmplPhone        :: !(Maybe Text)
    , tmplEmail        :: !(Maybe Text)
    , tmplLogin        :: !(Maybe Login)
    , tmplHRNumber     :: !(Maybe Int)
    }

employeeToTemplate :: Employee -> Tmpl
employeeToTemplate e = Tmpl
    { tmplPersonioId   = Nothing
    , tmplFirst        = e ^. employeeFirstName
    , tmplLast         = e ^. employeeLastName
    , tmplContractType = Just $ e ^. employeeContractType
    , tmplOffice       = e ^. employeeOffice
    , tmplTribe        = e ^. employeeTribe
    , tmplStartingDay  = Nothing
    , tmplSupervisor   = e ^. employeeSupervisor
    , tmplPhone        = e ^. employeePhone
    , tmplEmail        = e ^. employeeContactEmail
    , tmplLogin        = e ^. employeeFUMLogin
    , tmplHRNumber     = e ^. employeeHRNumber
    }

personioToTemplate :: Personio.Employee -> Tmpl
personioToTemplate e = Tmpl
    { tmplPersonioId   = Just $ e ^. Personio.employeeId
    , tmplFirst        = e ^. Personio.employeeFirst
    , tmplLast         = e ^. Personio.employeeLast
    , tmplContractType = contractType
    , tmplOffice       = e ^. Personio.employeeOffice
    , tmplTribe        = e ^. Personio.employeeTribe
    , tmplStartingDay  = e ^. Personio.employeeHireDate
    , tmplSupervisor   = "" -- TODO
    , tmplPhone        = e ^. Personio.employeeWorkPhone
    , tmplEmail        = Nothing
    , tmplLogin        = e ^. Personio.employeeLogin
    , tmplHRNumber     = e ^. Personio.employeeHRNumber
    }
  where
    contractType = case e ^. Personio.employeeEmploymentType of
        Nothing -> Nothing
        Just Personio.External -> Just ContractTypeExternal
        Just Personio.Internal -> case e ^. Personio.employeeContractType of
            Nothing                      -> Nothing
            Just Personio.PermanentAllIn -> Just ContractTypePermanent
            Just Personio.FixedTerm      -> Just ContractTypeFixedTerm
            Just Personio.Permanent      -> Nothing -- TODO!

-------------------------------------------------------------------------------
-- Page
-------------------------------------------------------------------------------

createEmployeePage
    :: World
    -> AuthUser    -- ^ logged in user
    -> Maybe Employee
    -> Maybe Personio.Employee
    -> HtmlPage "create-employee"
createEmployeePage world authUser memployee pemployee = checklistPage_ "Create employee" authUser $ do
    let tmpl = employeeToTemplate <$> memployee
            <|> personioToTemplate <$> pemployee
    -- Title
    header "Create employee" []

    for_ (tmplPersonioId =<< tmpl) $ \eid -> row_ $ large_ 12 $ do
        "Using personio employee #"
        toHtml eid
        " as template"
        hr_ []

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "employee-create" ] $ do
        input_
            [ futuId_ "employee-personio"
            , type_ "hidden"
            , value_ $ maybe "" toQueryParam $ tmpl >>= tmplPersonioId
            ]

        row_ $ large_ 12 $ label_ $ do
            "Checklist"
            -- TODO: sort checklists
            select_ [ futuId_ "employee-checklist" ] $ do
                optionSelected_ True [ value_ "" ] "-"
                forOf_ (worldLists . folded) world $ \l ->
                    optionSelected_ False
                        [ value_ $ l ^. identifierText ]
                        $ toHtml $ l ^. nameText
        row_ $ large_ 12 $ label_ $ do
            "First name"
            input_
                [ futuId_ "employee-firstname", type_ "text"
                , value_ $ maybe "" tmplFirst tmpl
                ]
        row_ $ large_ 12 $ label_ $ do
            "Last name"
            input_
                [ futuId_ "employee-lastname", type_ "text"
                , value_ $ maybe "" tmplLast tmpl
                ]
        row_ $ large_ 12 $ label_ $ do
            "Contract"
            let v = tmplContractType =<< tmpl
            select_ [ futuId_ "employee-contract-type" ] $ do
                optionSelected_ (v == Nothing) [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ (v == Just x)
                        [ value_ $ x ^. re _ContractType ]
                        $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Office"
            let v = tmplOffice <$> tmpl
            select_ [ futuId_ "employee-location" ] $ do
                optionSelected_ (v == Nothing) [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \x ->
                    optionSelected_ (v == Just x)
                        [ value_ $ x ^. re _Office ]
                        $ toHtml $ x ^. re _Office
        row_ $ large_ 12 $ label_ $ do
            "Confirmed"
            br_ []
            input_ [ futuId_ "employee-confirmed", type_ "checkbox" ]
        row_ $ large_ 12 $ label_ $ do
            "Due day"
            input_
                [ futuId_ "employee-starting-day"
                , type_ "date"
                , value_ $ maybe "" toQueryParam (tmpl >>= tmplStartingDay)
                ]
        row_ $ large_ 12 $ label_ $ do
            "Supervisor"
            input_
                [ futuId_ "employee-supervisor", type_ "text"
                , data_ "futu-values" $ encodeToText supervisors
                , value_ $ maybe "" (toQueryParam . tmplSupervisor) tmpl
                ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe"
            let v = tmplTribe <$> tmpl
            select_ [ futuId_ "employee-tribe", type_ "text" ] $ do
                optionSelected_ (v == Nothing) [ value_ "" ] "-"
                for_ [ minBound .. maxBound ] $ \tribe ->
                    optionSelected_ (v == Just tribe)
                        [ value_ $ tribeToText tribe ]
                        $ toHtml tribe
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info", rows_ "5" ] (pure ())
        row_ $ large_ 12 $ label_ $ do
            "Phone"
            input_
                [ futuId_ "employee-phone", type_ "tel"
                , value_ $ fromMaybe "" $ tmpl >>= tmplPhone
                ]
        row_ $ large_ 12 $ label_ $ do
            "Private email"
            input_
                [ futuId_ "employee-contact-email", type_ "email"
                , value_ $ fromMaybe "" $ tmpl >>= tmplEmail
                ]
        row_ $ large_ 12 $ label_ $ do
            "FUM handle"
            input_
                [ futuId_ "employee-fum-login", type_ "text"
                , value_ $ maybe "" toQueryParam $ tmpl >>= tmplLogin
                ]
        row_ $ large_ 12 $ label_ $ do
            "HR number"
            input_
                [ futuId_ "employee-hr-number", type_ "text"
                , value_ $ maybe "" textShow $ tmpl >>= tmplHRNumber
                ]

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] "Create"
            button_ [ class_ "button", data_ "futu-action" "reset" ] "Reset"
  where
    supervisors :: [Text]
    supervisors = toList $ setOf (worldEmployees . folded . employeeSupervisor . getter toQueryParam) world

encodeToText :: ToJSON a => a -> Text
encodeToText = view strict . encodeToLazyText
