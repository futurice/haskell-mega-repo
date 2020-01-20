{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.Logic where

import Data.Aeson       (object, (.=))
import Futurice.Email   (emailToText)
import Futurice.Prelude
import Prelude ()

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Okta      as O
import qualified Personio  as P

updateUsers :: (O.MonadOkta m) => [P.Employee] -> [O.User] -> m ()
updateUsers employees users = do
    let (_duplicates, singles) = Map.partition (\a -> length a > 1) personioMap
    let singles' = Map.mapMaybe listToMaybe singles
    let peopleToUpdate = catMaybes $ fmap (\(email, ouser) -> Map.lookup email singles' >>= changeData ouser) $ Map.toList loginMap

    -- update user information
    --void $ traverse (\c -> O.updateUser (O.oktaId c) (object ["secondEmail" .= T.strip (O.secondMail c)])) peopleToUpdate

    -- create new users
    traceShow notInactiveEmployeesNotInOkta $ pure ()
  where
    loginMap = Map.fromList $ (\u -> (O.getOktaLogin u, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    changeData ouser pemp = pemp ^. P.employeeHomeEmail >>= \email ->
      if not (T.null email) && Just email /= O.getSecondMail ouser then
        Just $ O.ChangeData (O.userId ouser) email
      else
        Nothing

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees
    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at email of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees
