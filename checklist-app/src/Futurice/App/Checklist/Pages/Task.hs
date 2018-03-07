{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

import Control.Lens              (contains, forOf_, lengthOf, re)
import Data.Time                 (diffDays)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import GitHub                    (SimpleUser)
import Prelude ()
import Servant.API               (safeLink)
import Web.HttpApiData           (toUrlPiece)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.Map        as Map
import qualified FUM.Types.Login as FUM
import qualified Futurice.IdMap  as IdMap
import qualified Personio        as P

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
taskPage
    :: World
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Task
    -> Vector SimpleUser
    -> Map P.EmployeeId P.Employee
    -> HashMap FUM.Login (P.Employee, PMUser)
    -> HtmlPage "task"
taskPage world today authUser task gemployees peremployees planemployees = checklistPage_ (view nameText task <> " - task") [] authUser (Just NavTasks) $ do
    row_ $ large_ 12 $ do
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ toUrlPiece
            $ safeLink checklistApi indexPageEndpoint
                  Nothing Nothing (task ^? identifier) defaultShowAll False
            ]
            "Goto employees listing"

    -- Edit
    row_ $ large_ 12 $ form_ [ futuId_ "task-edit", data_ "futu-task-id" $ task ^. identifierText ] $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                let v = task ^. nameText
                input_ [ futuId_ "task-name", type_ "text", value_ v ]
        row_ $ large_ 12 $
            label_ $ do
                "Info"
                input_ [ futuId_ "task-info", type_ "text", value_ $ task ^. taskInfo ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                select_ [ futuId_ "task-role" ] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == task ^. taskRole)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ role ^. re _TaskRole
        row_ $ large_ 12 $ label_ $ do
            "Comment field"
            br_ []
            checkbox_ (task ^. taskComment) [ futuId_ "task-comment" ]
        row_ $ large_ 12 $ label_ $ do
            "Prerequisites"
            br_ []
            small_ $ i_ "Note: Prerequisites must be also added to the checklist"
            select_ [ futuId_ "task-prereqs", multiple_ "multiple", size_ $ textShow (lengthOf (worldTasks . folded) world) ] $
                forOf_ (worldTasksSortedByName . folded) world $ \t -> do
                    optionSelected_ (task ^. taskPrereqs . contains (t ^. identifier))
                        [ value_ $ t ^. identifierText ]
                        $ toHtml $ t ^. nameText
        row_ $ large_ 12 $ label_ $ do
            "Tags"
            select_ [ futuId_ "task-tags", multiple_ "multiple" ] $
                for_ [minBound .. maxBound] $ \tag -> do
                    optionSelected_ (task ^. taskTags . contains tag)
                        [ value_ $ tag ^. re _TaskTag ]
                        $ toHtml tag

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "futu-action" "submit" ] $ "Save"
            button_ [ class_ "button", data_ "futu-action" "reset" ] $ "Reset"

    -- Employees
    subheader_ "Employees"
    row_ $ large_ 12 $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Office"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Check"]                       "Check"
            when (task ^. taskComment) $ th_           "Comment"
            th_ [title_ "Due date"]                    "Due date"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
        tbody_ $ for_ employees $ \employee -> tr_ $ do
            let startingDay = employee ^. employeeStartingDay
            td_ $ contractTypeHtml $ employee ^. employeeContractType
            td_ $ locationHtml (Nothing :: Maybe Checklist) $ employee ^. employeeOffice
            td_ $ employeeLink employee
            -- TODO: checklist link
            td_ $ checklistNameHtml world Nothing (employee ^. employeeChecklist) defaultShowAll
            td_ $ taskCheckbox_ world employee task
            unless (null $ task ^. taskTags) $ td_ $ taskInfo_ task (personioEmployee employee) (planmillEmployee employee) gemployees
            when (task ^. taskComment) $ td_ $ taskCommentInput_ world employee task
            td_ $ toHtml $ show startingDay
            td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
            td_ $ toHtml $ show (diffDays startingDay today) <> " days"

  where
    employees =  sortOn (view employeeStartingDay) $ toList $ Map.intersection
        (IdMap.toMap (world ^. worldEmployees))
        (world ^. worldTaskItems' .ix (task ^. identifier))

    personioEmployee :: Employee -> Maybe P.Employee
    personioEmployee employee = (employee ^. employeePersonio) >>= (\x -> peremployees ^.at x)

    planmillEmployee :: Employee -> Maybe PMUser
    planmillEmployee employee = do
            login <- checklistLogin <|> personioLogin
            snd <$> planemployees ^. at login
          where
            checklistLogin = employee ^. employeeFUMLogin
            personioLogin = do
                p <- personioEmployee employee
                p ^. P.employeeLogin
