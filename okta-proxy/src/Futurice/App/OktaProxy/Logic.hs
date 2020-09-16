{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.OktaProxy.Logic where

import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import Futurice.App.OktaProxy.Types

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified Okta            as O
import qualified Personio        as P

groupMembers :: (MonadOkta m, MonadPersonio m, MonadPlanMillQuery m, Monad m) => Text -> m [FUM.Login]
groupMembers groupName = do
    case O.groupMap ^.at groupName of
      Just group -> do
          oktaGroupMembers <- O.groupMembers $ O.giId group
          pure $ catMaybes $ (^. O.userProfile . O.profileFumUsername) <$> oktaGroupMembers
      Nothing -> pure []

userApplications :: (MonadOkta m) => P.EmployeeId -> m (Set AppResponse)
userApplications eid = do
    oktaUsers <- O.users
    let empMap = Map.fromList
            $ catMaybes
            $ (\o -> (,) <$> (P.EmployeeId <$> (readMaybe =<< o ^. O.userProfile . O.profileEmployeeNumber)) <*> (Just $ o ^. O.userId)) <$> oktaUsers
    case empMap ^.at eid of
      Just userId -> do
          userAppLinks <- O.userApplications userId
          appInformation <- traverse (O.application . O.alAppInstanceId) userAppLinks
          pure $ Set.fromList $ toAppResponse <$> appInformation
      Nothing -> pure mempty
  where
    toAppResponse app =
        AppResponse
        { appResLabel      = O.appLabel app
        , appResImageUrl   = listToMaybe $ O.linkLogoUrls $ O.app_links app
        }
