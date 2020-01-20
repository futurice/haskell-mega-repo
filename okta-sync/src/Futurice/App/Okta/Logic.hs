{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.Logic where

import Data.Aeson        (object, (.=))
import Futurice.Email    (emailToText)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Okta      as O
import qualified Personio  as P

data UpdateInformation = UpdateInformation
    { uiOktaId         :: !O.OktaId
    , uiSecondEmail    :: !Text
    , uiEmployeeNumber :: !P.EmployeeId
    } deriving Show

instance ToJSON UpdateInformation where
    toJSON i = object
        [ "secondEmail"    .= uiSecondEmail i
        , "employeeNumber" .= uiEmployeeNumber i
        ]

updateUsers :: (O.MonadOkta m) => [P.Employee] -> [O.User] -> m ()
updateUsers employees users = do
    let (_duplicates, singles) = Map.partition (\a -> length a > 1) personioMap
    let singles' = Map.mapMaybe listToMaybe singles
    let peopleToUpdate = catMaybes $ fmap (\(email, ouser) -> Map.lookup email singles' >>= changeData ouser) $ Map.toList loginMap

    -- update user information
    --void $ traverse (\c -> O.updateUser (uiOktaId c) (toJSON c)) peopleToUpdate

    -- create new users
    traceShow notInactiveEmployeesNotInOkta $ pure ()
    --traceShow peopleToUpdate $ pure ()
  where
    loginMap = Map.fromList $ (\u -> (O.getOktaLogin u, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    changeData ouser pemp = pemp ^. P.employeeHomeEmail >>= \email ->
      if not (T.null email) && Just email /= O.getSecondMail ouser then
        Just $ UpdateInformation (O.userId ouser) (T.strip email) (pemp ^. P.employeeId)
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

createUser :: (O.MonadOkta m) => P.Employee -> m O.User
createUser pemp = O.createUser newUser
  where
    newUser = O.NewUser
        { O.nuFirstName = pemp ^. P.employeeFirst
        , O.nuLastName = pemp ^. P.employeeLast
        , O.nuLogin = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuEmail = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuSecondEmail = fromMaybe "" $ pemp ^. P.employeeHomeEmail
        , O.nuPersonioNumber = pemp ^. P.employeeId
        , O.nuGithubUsername = fromMaybe "" $ textShow <$> pemp ^. P.employeeGithub
        }
