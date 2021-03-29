{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.FUM.Command.ResetPassword (ResetPassword) where

import Control.Lens      ((.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data ResetPassword (phase :: Phase) = ResetPassword
    { rpLogin    :: !Login
    , rpPassword :: !(Phased phase () Password)
    }
  deriving (Typeable, Generic)

deriveGeneric ''ResetPassword

instance phase ~ 'Input => HasLomake (ResetPassword phase) where
    lomake _ =
        dynEnumField "Login" :*
        unitField :*
        Nil

deriving via Sopica (ResetPassword phase) instance (phase ~ 'Internal) => ToJSON (ResetPassword phase)
deriving via Sopica (ResetPassword phase) instance (phase ~ 'Internal) => FromJSON (ResetPassword phase)
--deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (ResetPassword phase))   `Via` Sopica (ResetPassword phase) |]
--deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (ResetPassword phase)) `Via` Sopica (ResetPassword phase) |]

instance Command ResetPassword where
    type CommandTag ResetPassword = "reset-password"

    internalizeCommand now _login rights cmd = do
        requireRights RightsIT rights
        p <- liftIO $ makePassword sendSms now
        pure $ cmd
            { rpPassword = p
            }

    applyCommand _now login cmd = do
        validate login cmd

        let employee = rpLogin cmd
        worldEmployees . ix employee . employeePassword .= Just (rpPassword cmd)

        pure $ CommandResponseRedirect $ viewEmployeeHrefText login

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> ResetPassword phase -> m ()
validate _login cmd = do
    let login = rpLogin cmd

    unlessExists (worldEmployees . ix login) $
        throwError $ "Employee doesn't exist " ++ show (loginToText login)

-- | TODO: send sms of a clear text password
sendSms :: Text -> IO ()
sendSms _ = pure ()
