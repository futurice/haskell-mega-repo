{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillSync.Actions where

import Data.List                   (find)
import FUM.Types.Login             (Login, loginRegexp)
import Futurice.Prelude
import Futurice.Servant            (CommandResponse (..))
import Prelude ()
import Text.Regex.Applicative.Text (match)

import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Worker  as PM

import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.Monad

-- | Return date for update
canUpdateDepartDate :: P.Employee -> PM.User -> Either String Day
canUpdateDepartDate p pmu = do
    d <- maybe (Left "No end date in Personio") Right $ p ^. P.employeeEndDate
    if Just d == PM.uDepartDate pmu
    then Left $ "Depart date set to " ++ show d
    else Right d

updateDepartDate :: Ctx -> Login -> IO (CommandResponse ())
updateDepartDate ctx login = runLogT "update-depart-date" lgr $ runExceptT' $ do
    logInfo "Add depart date" login
    (p, pm) <- liftIO (fetchUser ctx login) >>= either throwError pure
    case canUpdateDepartDate p pm of
        Left err -> throwError $ "Non-updatable depart date: " ++ err
        Right d  -> do
            -- update depart date
            let pm0 = pm { PM.uDepartDate = Just d }

            let pm' = pm0
            logInfo "Editing PlanMill user" pm'

            -- Write to PM.
            let req = PM.editUser pm'
            res <- liftIO $ PM.submitPlanMillE (ctxWriteWorkers ctx) req

            case res of
                Left err -> do
                    logAttention "Failed PlanMill update" (show err)
                    throwError $ show err
                Right () -> return ()
  where
    lgr = ctxLogger ctx

    -- TODO: move to futurice-servant?
    runExceptT' :: Monad m => ExceptT String m a -> m (CommandResponse a)
    runExceptT' m = do
        e <- runExceptT m
        return $ case e of
            Left err -> CommandResponseError err
            Right x  -> CommandResponseOk x

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- | Note: maybe we want to return PMUser (from .Types)
fetchUser :: Ctx -> Login -> IO (Either String (P.Employee, PM.User))
fetchUser ctx login = runExceptT $ do
    ps <- liftIO $ runIntegrations' ctx $ P.personio P.PersonioEmployees
    p <- case find (\p -> p ^. P.employeeLogin == Just login) ps of
        Nothing -> throwError "Cannot find personio user"
        Just p  -> return p

    -- we submit to "write" workers, which always exist, though we read here
    -- (we are going to write!)
    epms <- liftIO $ PM.submitPlanMillE (ctxWriteWorkers ctx) PM.users
    pms <- case epms of
        Left err  -> throwError $ "Error fetching users from PlanMill:"  ++ show err
        Right pms -> return pms

    case find (\pm -> pmLogin pm == Just login) pms of
        Nothing -> throwError "Cannot find planmill user"
        Just u  -> return (p, u)

pmLogin :: PM.User -> Maybe Login
pmLogin u = match loginRe (PM.uUserName u)
  where
    loginRe = "https://login.futurice.com/openid/" *> loginRegexp
