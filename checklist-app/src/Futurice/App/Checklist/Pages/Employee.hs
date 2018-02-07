{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Control.Lens              (forOf_, re)
import Data.Aeson                (ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Set.Lens             (setOf)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API               (safeLink)
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.API
       (checklistApi, checklistPageEndpoint, createEmployeePageEndpoint,
       employeeAuditPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.TaskTag (taskTagToText)

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
    -> Map P.EmployeeId P.Employee
    -> HtmlPage "employee"
employeePage world authUser employee personios = checklistPage_ (view nameText employee) authUser $ do
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
            $ safeLink checklistApi createEmployeePageEndpoint (employee ^? identifier) Nothing False
            ]
            "Create employee using this employee as a template"
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
            "Contract "
            hasDifferentPersonioContractType (personioEmployee personios (employee ^. employeePersonio)) (Just ((employee ^. employeeContractType) ^. re _ContractType))
            select_ [ futuId_ "employee-contract-type" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeContractType)
                    [ value_ $ x ^. re _ContractType ]
                    $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Office "
            hasDifferentPersonioOffice (personioEmployee personios (employee ^. employeePersonio)) (Just ((employee ^. employeeOffice) ^. re _Office))
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
            "Supervisor "
            hasDifferentPersonioSupervisor (personioEmployee personios (employee ^. employeePersonio)) (Just $ toQueryParam $ employee ^. employeeSupervisor) personios
            input_ [ futuId_ "employee-supervisor", type_ "text", value_ $ toQueryParam $ employee ^. employeeSupervisor, data_ "futu-values" $ encodeToText supervisors ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe "
            hasDifferentPersonioTribe (personioEmployee personios (employee ^. employeePersonio)) (Just $ toQueryParam $ employee ^. employeeTribe)
            select_ [ futuId_ "employee-tribe", type_ "text" ] $ do
                for_ [ minBound .. maxBound ] $ \tribe ->
                    optionSelected_ (tribe == employee ^. employeeTribe)
                        [ value_ $ toQueryParam tribe ]
                        $ toHtml tribe
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info", rows_ "5" ] $ toHtml $ employee ^. employeeInfo
        row_ $ large_ 12 $ label_ $ do
            "Phone "
            hasDifferentPersonioPhone (personioEmployee personios (employee ^. employeePersonio)) (employee ^. employeePhone)
            -- TODO: maybe it's simpler to just define empty value
            input_ $ [ futuId_ "employee-phone", type_ "tel" ] ++
                catMaybes [ value_ <$> employee ^. employeePhone ]
        row_ $ large_ 12 $ label_ $ do
            "Private email "
            hasDifferentPersonioEmail (personioEmployee personios (employee ^. employeePersonio)) (employee ^. employeeContactEmail)
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
                td_ $ do
                    taskLink task
                    for_ (task ^. taskTags) $ \tag -> do
                        br_ []
                        case tag of
                           -- temporary solution. Real tag related information will replace these
                          GithubTask -> toHtml $ taskTagToText tag
                          PlanmillTask -> toHtml $ taskTagToText tag
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

    personioEmployee :: Map P.EmployeeId P.Employee -> Maybe P.EmployeeId -> Maybe P.Employee
    personioEmployee _ Nothing = Nothing
    personioEmployee m (Just i) = m ^.at i

    personioText :: Maybe Text -> Text -> Text
    personioText a attr = maybe "" (\x -> " Personio " <> attr <> " " <> x) a

    wrapToWarningLabel :: Text -> HtmlT Identity ()
    wrapToWarningLabel x = label_ [ class_ "label warning" ] (toHtml x)

    contractType :: P.Employee -> Maybe ContractType
    contractType e = case e ^. P.employeeEmploymentType of
        Nothing -> Nothing
        Just P.External -> Just ContractTypeExternal
        Just P.Internal -> case e ^. P.employeeContractType of
            Nothing                      -> Nothing
            Just P.PermanentAllIn -> Just ContractTypePermanent
            Just P.FixedTerm      -> Just ContractTypeFixedTerm
            Just P.Permanent      -> Nothing -- TODO!

    hasDifferentPersonioInfo :: Maybe Text -> Maybe Text -> Text -> HtmlT Identity ()
    hasDifferentPersonioInfo info Nothing t = wrapToWarningLabel $ personioText info t
    hasDifferentPersonioInfo info locali t = if info == locali then "" else wrapToWarningLabel $ personioText info t

    hasDifferentPersonioPhone :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioPhone info locali = maybe (pure ()) (\x -> hasDifferentPersonioInfo (x ^. P.employeeHomePhone) locali "phone") info

    hasDifferentPersonioEmail :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioEmail info locali = maybe (pure ()) (\x -> hasDifferentPersonioInfo (x ^. P.employeeHomeEmail) locali "email") info

    hasDifferentPersonioTribe :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioTribe info locali = maybe (pure ()) (\x -> hasDifferentPersonioInfo (Just $ toQueryParam $ x ^. P.employeeTribe) locali "tribe") info

    hasDifferentPersonioContractType :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioContractType info locali = maybe (pure ()) (\x -> hasDifferentPersonioInfo (toQueryParam <$> contractType x) locali "contract") info

    hasDifferentPersonioOffice :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioOffice info locali = maybe (pure ()) (\x -> hasDifferentPersonioInfo (Just $ toQueryParam $ x ^. P.employeeOffice) locali "office") info

    hasDifferentPersonioSupervisor :: Maybe P.Employee -> Maybe Text -> Map P.EmployeeId P.Employee -> HtmlT Identity ()
    hasDifferentPersonioSupervisor info locali es = let toName :: P.Employee -> Maybe Text
                                                        toName x = do
                                                            suid <- x ^. P.employeeSupervisorId
                                                            es ^? ix suid . P.employeeFullname
                                                    in maybe (pure ()) (\x -> hasDifferentPersonioInfo (toName x) locali "supervisor") info

encodeToText :: ToJSON a => a -> Text
encodeToText = view strict . encodeToLazyText
