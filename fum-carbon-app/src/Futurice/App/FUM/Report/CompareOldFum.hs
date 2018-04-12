{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.FUM.Report.CompareOldFum (compareOldFumReport) where

import Control.Concurrent.STM    (readTVarIO)
import Control.Lens              (iforOf_)
import Data.Maybe                (isNothing)
import Data.These                (_That, _These, _This)
import Futurice.Integrations     (fumEmployeeList, runIntegrations)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types

import qualified Data.Map as Map
import qualified FUM
import qualified Personio

compareOldFumReport :: Ctx -> IO (HtmlPage "compare-old-fum-report")
compareOldFumReport ctx = do
    let mgr = ctxManager ctx
    let lgr = ctxLogger ctx
    let integrCfg = cfgIntegrationsConfig $ ctxConfig ctx
    now <- currentTime
    let today = utctDay now

    ps' <- readTVarIO $ ctxPersonio ctx
    let ps = filter (Personio.employeeIsActive today) (toList ps')
    fs <- runIntegrations mgr lgr now integrCfg (toList <$> fumEmployeeList)

    let loginPs :: Map Login Personio.Employee
        loginPs = Map.fromList
            $ mapMaybe (\e -> (,e) <$> e ^. Personio.employeeLogin)
            $ ps

    let loginFs :: Map Login FUM.User
        loginFs = Map.fromList
            $ map (\e -> (e ^. FUM.userName, e))
            $ fs

    let loginPs' :: Map Login Personio.Employee
        loginPs' = Map.fromList
            $ mapMaybe (\e -> (,e) <$> e ^. Personio.employeeLogin)
            $ toList ps'

    let aligned :: Map Login (These Personio.Employee FUM.User)
        aligned = align loginPs loginFs

    pure $ page_ "Personio ⇔ FUM5 cross check" $ do
        fullRow_ $ h1_ "Personio ⇔ FUM5 cross check"

        fullRow_ $ h2_ "People with non-matching emails"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Personio"
                th_ "Name"
                th_ "FUM email"
                th_ "Personio email"
            tbody_ $ iforOf_ (ifolded . _These) aligned $ \login (p, f) -> do
                let emailsMatch = p ^? Personio.employeeEmail . _Just . getter emailToText == f ^. FUM.userEmail . lazy
                when (isNothing (p ^. Personio.employeeEmail) || not emailsMatch) $ tr_ $ do
                    td_ $ toHtml login
                    td_ $ toHtml $ p ^. Personio.employeeId
                    td_ $ toHtml $ f ^. FUM.userFullName
                    td_ $ traverse_ toHtml $ f ^. FUM.userEmail
                    td_ $ traverse_ toHtml $ p ^. Personio.employeeEmail

        fullRow_ $ h2_ "Old FUM status cross-check"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Name"
                th_ "(FUM) Status"
                th_ "Google Status"
                th_ "Active In PlanMill"
                th_ "Personio"

            tbody_ $ ifor_ loginFs $ \login u -> do
                let usToGoogle FUM.StatusActive   = "activeperson"
                    usToGoogle FUM.StatusDisabled = "suspended"
                    usToGoogle FUM.StatusDeleted  = "deleted"

                let status  = u ^. FUM.userStatus
                    gStatus = u ^. FUM.userGoogleStatus
                    pmStatus =  u ^. FUM.userActiveInPm

                    gStatusOk = gStatus ^. lazy == Just (usToGoogle status)

                unless (gStatusOk && status == FUM.StatusActive) $ tr_ $ do
                    td_ $ toHtml login
                    td_ $ toHtml $ u ^. FUM.userFullName
                    td_ $ toHtml $ show status
                    td_ $ toHtml $ show gStatus
                    td_ $ toHtml $ show pmStatus
                    td_ $ for_ (loginPs' ^? ix login) $ \p -> do
                        toHtml $ p ^. Personio.employeeId
                        " "
                        toHtml $ show $ p ^. Personio.employeeStatus

        fullRow_ $ h2_ "People with non-matching phone numbers"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Personio"
                th_ "Name"
                th_ "FUM phone"
                th_ "Personio phone"
            tbody_ $ iforOf_ (ifolded . _These) aligned $ \login (p, f) -> do
                let phonesMatch = p ^? Personio.employeeWorkPhone . _Just == f ^. FUM.userPhone1 . lazy
                when (isNothing (p ^. Personio.employeeWorkPhone) || not phonesMatch) $ tr_ $ do
                    td_ $ toHtml login
                    td_ $ toHtml $ p ^. Personio.employeeId
                    td_ $ toHtml $ f ^. FUM.userFullName
                    td_ $ traverse_ toHtml $ f ^. FUM.userPhone1
                    td_ $ traverse_ toHtml $ p ^. Personio.employeeWorkPhone

        fullRow_ $ h2_ "Active people in Personio without FUM login"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Personio"
                th_ "Name"
                th_ "Personio email"
            tbody_ $ for_ ps $ \p -> do
                when (isNothing $ p ^. Personio.employeeLogin) $ tr_ $ do
                    td_ $ toHtml $ p ^. Personio.employeeId
                    td_ $ toHtml $ p ^. Personio.employeeFullname
                    td_ $ traverse_ toHtml $ p ^. Personio.employeeEmail

        fullRow_ $ h2_ "Active people in FUM, but not in Personio"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Name"
                th_ "FUM email"
            tbody_ $ iforOf_ (ifolded . _That) aligned $ \login f -> tr_ $ do
                td_ $ toHtml login
                td_ $ toHtml $ f ^. FUM.userFullName
                td_ $ traverse_ toHtml $ f ^. FUM.userEmail

        fullRow_ $ do
            h2_ "Active people in Personio, but not in FUM"
            em_ "And have login set!"
        fullRow_ $ table_ $ do
            thead_ $ tr_ $ do
                th_ "Login"
                th_ "Personio"
                th_ "Name"
                th_ "Personio email"
            tbody_ $ iforOf_ (ifolded . _This) aligned $ \login p -> tr_ $ do
                td_ $ toHtml login
                td_ $ toHtml $ p ^. Personio.employeeId
                td_ $ toHtml $ p ^. Personio.employeeFullname
                td_ $ traverse_ toHtml $ p ^. Personio.employeeEmail
