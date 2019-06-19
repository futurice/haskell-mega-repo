{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Pages.ViewEmployee (viewEmployeePage) where

import Control.Lens     (contains)
import Futurice.IdMap   (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command
import Futurice.App.FUM.Lomake
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio

viewEmployeePage
    :: AuthUser
    -> World     -- ^ the world
    -> IdMap Personio.Employee
    -> UTCTime   -- ^ now
    -> Employee  -- ^ employees
    -> HtmlPage "view-employee"
viewEmployeePage auth world personio now e = fumPage_ "Employee" auth $ do
    let login = e ^. employeeLogin
    let mp = personio ^? ix (e ^. employeePersonioId)

    todos_ [ "picture", "its editing" ]

    -- Title
    fumHeader_ "Employee" [Just $ loginToText login]

    fullRow_ $ table_ $ tbody_ $ do
        vertRow_ "Name" $ toHtml $ e ^. employeeName
        vertRow_ "Login" $ toHtml login
        vertRow_ "Personio ID" $ toHtml $ e ^. employeePersonioId
        vertRow_ "Status" $ toHtml $ e ^. employeeStatus

        mcase mp (vertRow_ "Personio" $ em_ "cannot find id") $ \p -> do
            vertRow_ "Office"   $ toHtml $ p ^. Personio.employeeOffice
            vertRow_ "Tribe"    $ toHtml $ p ^. Personio.employeeTribe
            vertRow_ "Phone"    $ traverse_ phoneToHtml $ p ^. Personio.employeeWorkPhone
            vertRow_ "Email"    $ traverse_ toHtml $ p ^. Personio.employeeEmail
            vertRow_ "Int/Ext"  $ traverse_ toHtml $ p ^. Personio.employeeEmploymentType
            vertRow_ "GitHub"   $ traverse_ toHtml $ p ^. Personio.employeeGithub
            --
            when (authRights auth == RightsIT || authLogin auth == login) $ do
                vertRow_ "Contract span" $ toHtml $ formatDateSpan
                    (p ^. Personio.employeeHireDate)
                    (p ^. Personio.employeeEndDate)

    block_ "Groups" $ do
        let sgroups = mcase mp mempty $ \p ->
                world ^.. worldSpecialGroups
                    . ix (p ^. Personio.employeeEmploymentType, p ^. Personio.employeeOffice, p ^. Personio.employeeTribe)
                    . folded

        let groups = sortOn (view groupName) $ sgroups
                <> world ^.. worldEmployeeGroups . ix login . folded

        when (null groups) $
            row_ $ large_ 12 [ class_ "callout warning" ] $
                em_ "No groups"

        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Name"
                th_ "Type"
                th_ mempty

            tbody_ $ for_ groups $ \g -> tr_ $ do
                td_ $ a_ [ viewGroupHref_ $ g ^. groupName] $ toHtml $ g ^. groupName
                td_ $ toHtml $ g ^. groupType
                td_ $ case g ^. groupEmployees . contains login of
                    True -> commandHtmlSubmit (Proxy :: Proxy RemoveEmployeeFromGroup) "Remove" "warning" $
                        vHidden (g ^. groupName) :*
                        vHidden login :*
                        Nil
                    False -> "Automatic membership"

        subheader_ "Add to group"
        commandHtmlSubmit (Proxy :: Proxy AddEmployeeToGroup) "Add to group" "success" $
            vGroups (\gn -> canEditGroup login gn world) world :*
            vHidden login :*
            Nil

    block_ "Email aliases" $ do
        when (null $ e ^. employeeEmailAliases) $
            row_ $ large_ 12 [ class_ "callout warning" ] $
                em_ "No email aliases"

        fullRow_ $ table_ $ tbody_ $
            for_ (e ^.. employeeEmailAliases . folded) $ \email -> tr_ $ do
                td_ $ toHtml email
                td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveEmailFromEmployee) "Remove" "warning" $
                    vHidden login :*
                    vHidden email :*
                    Nil

        subheader_ "Add email alias"
        commandHtmlSubmit (Proxy :: Proxy AddEmailToEmployee) "Add email address" "success" $
            vHidden login :*
            vNothing :*
            Nil

    block_ "SSH Keys" $ do
        unless (null $ e ^. employeeSshKeys) $ fullRow_ $ table_ $ tbody_ $
            for_ (e ^. employeeSshKeys) $ \sshKey -> tr_ $ do
                td_ $ toHtml sshKey
                td_ $ commandHtmlSubmit (Proxy :: Proxy RemoveSSHKeyFromEmployee) "Remove" "warning" $
                    vHidden login :*
                    vHidden (sshKey ^. sshKeyFingerprint) :*
                    Nil

        subheader_ "Add SSH Key"
        commandHtmlSubmit (Proxy :: Proxy AddSSHKeyToEmployee) "Add SSH Key" "success" $
            vHidden login :*
            vNothing :*
            Nil

    when (authRights auth == RightsIT || authLogin auth == e ^. employeeLogin) $ do
        block_ "Password" $ do
            fullRow_ $ case e ^. employeePassword of
                Nothing -> span_ [ class_ "warning" ] "No password"
                Just p  -> passwordToHtml now p

            todos_ [ "Change of own password" ]

    when (authRights auth == RightsIT) $ block_ "Admin" $ do
        fullRow_ $ div_ [ class_ "button-group" ] $ do
            commandHtmlSubmit (Proxy :: Proxy ResetPassword) "Reset password" "warning" $
                vHidden login :*
                vNothing :*
                Nil

            -- button_ [ class_ "button alert" ] "Remove password"

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- same as planmill-sync
formatDateSpan :: Maybe Day -> Maybe Day -> String
formatDateSpan s e =
    let s' = maybe "?" show s
        e' = maybe arrow (\x -> ndash ++ show x) e
    in s' ++ e'
  where
    ndash = "–"
    arrow = " →" -- space is intentional

phoneToHtml :: Monad m => Text -> HtmlT m ()
phoneToHtml t = a_ [ href_ $ "tel://" <> t ] $ toHtml t
