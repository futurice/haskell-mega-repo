{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.Logic where

import Data.Aeson       (object, (.=))
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
    void $ traverse (\c -> O.updateUser (O.oktaId c) (object ["secondEmail" .= T.strip (O.secondMail c)])) peopleToUpdate
  where
    loginMap = Map.fromList $ (\u -> (O.getOktaLogin u, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    changeData ouser pemp = pemp ^. P.employeeHomeEmail >>= \email ->
      if not (T.null email) && Just email /= O.getSecondMail ouser then
        Just $ O.ChangeData (O.userId ouser) email
      else
        Nothing
