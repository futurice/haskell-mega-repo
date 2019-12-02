{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Employee (employeePage) where

import Control.Lens              (Getting, forOf_, re)
import Data.Aeson                (ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Set.Lens             (setOf)
import Data.Time                 (addDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API               (safeLink)
import Web.HttpApiData           (toQueryParam)

import Futurice.App.Checklist.API
       (checklistApi, checklistPageEndpoint, createEmployeePageEndpoint,
       employeeAuditPageEndpoint)
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Personio
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
    -> IntegrationData
    -> HtmlPage "employee"
employeePage world authUser employee integrationData = checklistPage_ (view nameText employee) [] authUser Nothing $ do
    -- Buttons
    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        for_ (employee ^. employeePersonio) $ \pid ->
            toHtml pid `with` [ class_ " button hollow" ]
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi checklistPageEndpoint (cl ^. checklistId)
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
            $ safeLink checklistApi createEmployeePageEndpoint (Just LeavingEmployeeChecklist) (employee ^? identifier) Nothing
            ]
            "Create employee using this employee as a template"
        button_
            [ class_ "button alert"
            , futuId_ "employee-archive"
            , data_ "futu-employee-id" $ employee ^. identifierText
            ]
            "Archive employee"

    -- Personio info
    for_ personioEmployee $ \p -> fullRow_ $ condensedTable_ $ tbody_ $ do
        -- TODO: this could be shared with fum-carbon
        vertRow_ "Name"       $ toHtml $ p ^. P.employeeFullname
        vertRow_ "Office"     $ toHtml $ p ^. P.employeeOffice
        vertRow_ "Role"       $ toHtml $ p ^. P.employeeRole
        vertRow_ "Supervisor" $ traverse_ toHtml $ do
            sid <- p ^. P.employeeSupervisorId
            s   <- integrationData ^? personioData . ix sid
            return (s ^. P.employeeFullname)
        vertRow_ "Tribe"      $ toHtml $ p ^. P.employeeTribe
        vertRow_ "CC"         $ traverse_ toHtml $ p ^. P.employeeCostCenter
        vertRow_ "Phone"      $ traverse_ phoneToHtml $ p ^. P.employeeWorkPhone
        vertRow_ "Email"      $ traverse_ toHtml $ p ^. P.employeeEmail
        vertRow_ "Int/Ext"    $ traverse_ toHtml $ p ^. P.employeeEmploymentType
        vertRow_ "HR Number"  $ traverse_ (toHtml . show) $ p ^. P.employeeHRNumber
        vertRow_ "FUM"        $ traverse_ toHtml $ p ^. P.employeeLogin
        vertRow_ "GitHub"     $ traverse_ toHtml $ p ^. P.employeeGithub
        vertRow_ "Private phone"   $ traverse_ toHtml $ p ^. P.employeeHomePhone
        vertRow_ "Private email"   $ traverse_ toHtml $ p ^. P.employeeHomeEmail

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
            personioCheck' employeeContractType contractType (toHtml . contractTypeToText)
            select_ [ futuId_ "employee-contract-type" ] $ for_ [ minBound .. maxBound ] $ \x ->
                optionSelected_ (x == employee ^. employeeContractType)
                    [ value_ $ x ^. re _ContractType ]
                    $ toHtml $ x ^. re _ContractType
        row_ $ large_ 12 $ label_ $ do
            "Office "
            personioCheck' employeeOffice (Just . view P.employeeOffice) toHtml
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
            hasDifferentPersonioSupervisor personioEmployee (Just $ toQueryParam $ employee ^. employeeSupervisor) (integrationData ^. personioData)
            input_ [ futuId_ "employee-supervisor", type_ "text", value_ $ toQueryParam $ employee ^. employeeSupervisor, data_ "futu-values" $ encodeToText supervisors ]
        row_ $ large_ 12 $ label_ $ do
            "Tribe "
            personioCheck' employeeTribe (Just . view P.employeeTribe) toHtml
            select_ [ futuId_ "employee-tribe", type_ "text" ] $ do
                for_ (sortOn tribeToText [ minBound .. maxBound ]) $ \tribe ->
                    optionSelected_ (tribe == employee ^. employeeTribe)
                        [ value_ $ toQueryParam tribe ]
                        $ toHtml tribe
        row_ $ large_ 12 $ label_ $ do
            "Info"
            textarea_ [ futuId_ "employee-info", rows_ "5" ] $ toHtml $ employee ^. employeeInfo
        row_ $ large_ 12 $ label_ $ do
            "Private Phone "
            hasDifferentPersonioPhone personioEmployee (employee ^. employeePhone)
            -- TODO: maybe it's simpler to just define empty value
            input_ $ [ futuId_ "employee-phone", type_ "tel" ] ++
                catMaybes [ value_ <$> employee ^. employeePhone ]
        row_ $ large_ 12 $ label_ $ do
            "Private email "
            hasDifferentPersonioEmail personioEmployee (employee ^. employeeContactEmail)
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
        row_ $ large_ 12 $ label_ $ do
            "Add notice sign"
            br_ []
            checkbox_ (fromMaybe False $ employee ^. employeeNotice) [ futuId_ "employee-notice" ]
        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Save"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Tasks
    subheader_ "Tasks"
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [ title_ "Task" ]  "Task"
            th_ [ title_ "Role" ]  "Role"
            th_ [ title_ "Check" ] "Check"
            th_ [ title_ "Infot" ] "Info"
            th_ [ title_ "Comment" ] "Comment"
            th_ [ title_ "Due date" ] "Due date"
            th_ [ title_ "Who and when have done this task" ] "Audit"
        tbody_ $ forOf_ (worldTasksSorted (authUser ^. authUserTaskRole) . folded) world $ \task -> do
            let tid = task ^. identifier
            for_ (world ^? worldTaskItems . ix eid . ix tid) $ \taskItem -> tr_ $ do
                td_ $ taskLink task
                td_ $ roleHtml (employee ^? employeeChecklist) (task ^. taskRole)
                td_ $ taskCheckbox_ world employee task
                td_ $ taskInfo_ task employee integrationData
                td_ $ taskCommentInput_ world employee task
                td_ $ day'_ $ addDays (task ^. taskOffset) (employee ^. employeeStartingDay)
                td_ $ forOf_ _AnnTaskItemDone taskItem $ \(_, fumUser, timestamp) -> do
                    toHtml fumUser
                    " "
                    toHtml $ show $ localDay $ utcToHelsinkiTime timestamp

    subheader_ "DANGER"
    when (authUser ^. _2 == TaskRoleIT) $ row_ $ large_ 12 $ do
        hr_ []
        button_
            [ class_ "button alert"
            , futuId_ "employee-remove"
            , data_ "futu-employee-id" $ employee ^. identifierText
            ] "DELETE EMPLOYEE"
  where
    eid = employee ^. identifier
    cl = world ^. worldLists . pick (employee ^. employeeChecklist)

    personioCheck'
        :: (Monad m, Eq a)
        => Getting a Employee a
        -> (P.Employee -> Maybe a)
        -> (a -> HtmlT m ())
        -> HtmlT m ()
    personioCheck' = personioCheck integrationData employee

    supervisors :: [Text]
    supervisors = toList $ setOf (worldEmployees . folded . employeeSupervisor . getter toQueryParam) world

    personioEmployee :: Maybe P.Employee
    personioEmployee = do
        pid <- employee ^. employeePersonio
        integrationData ^. personioData . at pid

    personioText :: Maybe Text -> Text -> Text
    personioText a attr = maybe "" (\x -> " Personio " <> attr <> " " <> x) a

    wrapToWarningLabel :: Text -> HtmlT Identity ()
    wrapToWarningLabel x = label_ [ class_ "label warning" ] (toHtml x)

    hasDifferentPersonioInfo :: Maybe Text -> Maybe Text -> Text -> HtmlT Identity ()
    hasDifferentPersonioInfo info Nothing t = wrapToWarningLabel $ personioText info t
    hasDifferentPersonioInfo info locali t = if info == locali then "" else wrapToWarningLabel $ personioText info t

    hasDifferentPersonioPhone :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioPhone info locali = for_ info $ \x -> hasDifferentPersonioInfo (x ^. P.employeeHomePhone) locali "phone"

    hasDifferentPersonioEmail :: Maybe P.Employee -> Maybe Text -> HtmlT Identity ()
    hasDifferentPersonioEmail info locali = for_ info $ \x -> hasDifferentPersonioInfo (x ^. P.employeeHomeEmail) locali "email"

    hasDifferentPersonioSupervisor :: Maybe P.Employee -> Maybe Text -> Map P.EmployeeId P.Employee -> HtmlT Identity ()
    hasDifferentPersonioSupervisor info locali es = let toName :: P.Employee -> Maybe Text
                                                        toName x = do
                                                            suid <- x ^. P.employeeSupervisorId
                                                            es ^? ix suid . P.employeeFullname
                                                    in for_ info $ \x -> hasDifferentPersonioInfo (toName x) locali "supervisor"

encodeToText :: ToJSON a => a -> Text
encodeToText = view strict . encodeToLazyText

phoneToHtml :: Monad m => Text -> HtmlT m ()
phoneToHtml t = a_ [ href_ $ "tel://" <> t ] $ toHtml t
