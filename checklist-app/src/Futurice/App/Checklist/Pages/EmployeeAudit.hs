{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.EmployeeAudit (employeeAuditPage) where

import Control.Lens               (ALens, use, ( #~ ), (.=), (^#))
import Control.Monad.State.Strict (State, evalState)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.API                (safeLink)

import Futurice.App.Checklist.API     (checklistApi, employeePageEndpoint)
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM.Types.Login as FUM

employeeAuditPage
    :: World
    -> AuthUser
    -> Employee
    -> [(Command Identity, FUM.Login, UTCTime)]
    -> HtmlPage "employee-audit"
employeeAuditPage world authUser employee cmds = checklistPage_ (view nameText employee) [] authUser Nothing $ do
    let eid = employee ^. identifier

    row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
        button_
            [ class_ "button"
            , data_ "futu-link-button" $ linkToText
            $ safeLink checklistApi employeePageEndpoint $ employee ^. identifier
            ]
            "Employee page"

    row_ $ large_ 12 $ table_ $ do
        tr_ $ do
            th_ "Who"
            th_ "When"
            th_ "What"

        for_ (processCommands world eid _1 cmds) $ \(cmdHtml, username, timestamp) -> tr_ $ do
            td_ $ toHtml username
            td_ [ style_ "white-space: nowrap" ] $
                toHtml $ formatHumanHelsinkiTime timestamp
            td_ cmdHtml

processCommands
    :: forall s t.
       World
    -> Identifier Employee
    -> ALens s t (Command Identity) (Html ())
    -> [s]
    -> [t]
processCommands world eid l = concat . flip evalState False . traverse process
  where
    taskHtml tid = maybe (toHtml $ tid ^. identifierText) taskLink $
        world ^? worldTasks . ix tid

    process :: s -> State Bool [t]
    process x = case x ^# l of
        CmdCreateEmployee (Identity eid') _ edit
            -- if created this employee
            | eid == eid' -> do
                id .= True

                let html = do
                      b_ "Create employee"
                      hr_ []
                      toHtml edit

                pure [ x & l #~ html]

            -- created someone else, we don't care
            | otherwise -> pure []

        CmdEditEmployee eid' edit
            | eid == eid' -> do
                let html = do
                      b_ "Edit employee"
                      hr_ []
                      toHtml edit

                pure [ x & l #~ html ]

            -- created someone else, we don't care
            | otherwise -> pure []

        CmdTaskItemToggle _ tid d -> do
            -- showing always
            let html = do
                  b_ $ if d == TaskItemDone then "Task done" else "Task undone"
                  " "
                  taskHtml tid
            pure [ x & l #~ html ]

        CmdTaskEditComment _ tid comment -> do
            -- showing always
            let html = do
                  b_ "Task commented"
                  " "
                  taskHtml tid
                  ": "
                  i_ $ toHtml comment
            pure [ x & l #~ html ]

        CmdAddTask _ tid -> do
            created <- use id

            let html = do
                  b_ "Added task"
                  " "
                  taskHtml tid

                  -- TODO: show only if current state of user applies
                  "; "
                  em_ "May not apply to this user"

            pure $ if created then [x & l #~ html ] else []

        CmdRemoveTask _ tid -> do
            created <- use id

            let html = do
                  b_ "Removed task"
                  " "
                  taskHtml tid
                  -- TODO: show only if current state of user applies
                  "; "
                  em_ "May not apply to this user"

            pure $ if created then [ x & l #~ html ] else []

        CmdArchiveEmployee eid' _
            | eid == eid' -> do
                id .= False
                pure [ x & l #~ b_ "Archived user" ]
            | otherwise -> pure []

        -- Default
        cmd -> do
            created <- use id
            let html = toHtml $ textShow cmd
            pure $ if created then [x & l #~ html ] else []
