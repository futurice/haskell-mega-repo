{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.IndexPage where

import Data.Aeson                (toJSON)
import Futurice.Email            (emailToText)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Okta.Markup

import qualified Data.Map as Map
import qualified Okta     as O
import qualified Personio as P

indexPage :: [P.Employee] -> [O.User] -> HtmlPage "indexpage"
indexPage employees users = page_ "Okta sync" (Just NavHome) $ do
    fullRow_ $ do
        h2_ "Not inactive people in Personio that are not in Okta"
        sortableTable_ $ do
            thead_ $ do
                th_ mempty
                th_ "Name"
                th_ "Employment"
            tbody_ $ do
                for_ notInactiveEmployeesNotInOkta $ \e -> tr_ $ do
                    td_ $ checkbox_ False [ data_ "okta-add-user" $ employeeNumber $ e ^. P.employeeId]
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ toHtml $ maybe "" show (e ^. P.employeeEmploymentType)
        div_ [ class_ "button-group" ] $
            button_ [ id_ "add-users", class_ "button alert", disabled_ "disabled" ] "Add to Okta"
        h2_ "People in Okta that are not in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Login"
            tbody_ $ do
                for_ oktaUsersNotInPersonio $ \u -> tr_ $ do
                    td_ $ toHtml $ (O.profileFirstName $ O.userProfile u) <> " " <> O.profileLastName (O.userProfile u)
                    td_ $ toHtml $ O.profileLogin $ O.userProfile u
        h2_ "All employees in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Okta status"
            tbody_ $ do
                for_ employees $ \e -> tr_ $ do
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ for_ (e ^. P.employeeEmail >>= \email -> loginMap ^. at email) $ \u -> do
                        toHtml $ show $ O.userStatus u
  where
    employeeNumber (P.EmployeeId n) = textShow n
    loginMap = Map.fromList $ (\u -> (O.getOktaLogin u, u)) <$> users
    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees
    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at email of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

    oktaUsersNotInPersonio = filter (\u -> case personioMap ^. at (O.getOktaLogin u) of
                                             Just _ -> False
                                             Nothing -> True) users
