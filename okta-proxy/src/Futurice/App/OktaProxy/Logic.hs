{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.OktaProxy.Logic where

import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified Okta            as O

groupMembers :: (MonadOkta m, MonadPersonio m, MonadPlanMillQuery m, Monad m) => Text -> m [FUM.Login]
groupMembers groupName = do
    case O.groupMap ^.at groupName of
      Just group -> do
          oktaGroupMembers <- O.groupMembers $ O.giId group
          pure $ catMaybes $ (^. O.userProfile . O.profileFumUsername) <$> oktaGroupMembers
      Nothing -> pure []
