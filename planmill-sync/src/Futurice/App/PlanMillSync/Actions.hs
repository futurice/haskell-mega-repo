{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillSync.Actions where

import Data.List                   (find)
import Data.Tuple                  (swap)
import FUM.Types.Login             (Login, loginRegexp)
import Futurice.Prelude
import Futurice.Servant            (CommandResponse (..))
import Prelude ()
import Text.Regex.Applicative.Text (match)

import qualified Data.Map.Strict  as Map
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ
import qualified PlanMill.Worker  as PM

import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.Monad
import Futurice.App.PlanMillSync.Types

-------------------------------------------------------------------------------
-- Status
-------------------------------------------------------------------------------

canUpdateStatus :: Day -> P.Employee -> PMUser -> Either String Text
canUpdateStatus today p pm
    | pActiveT == pmPassive pm = Left $ "Statuses are the same: " ++ show pActiveT
    | otherwise                = Right pActiveT
  where
    pActive = P.employeeIsActive today p
    pActiveT = if pActive then "Active" else "Passive"

updateStatus :: Ctx -> Login -> IO (CommandResponse ())
updateStatus ctx login = runLogT "update-status" lgr $ runExceptT' $ do
    logInfo "Update status" login
    D today p pm statusMap _ <- liftIO (fetchUser ctx login) >>= either throwError pure
    case canUpdateStatus today p pm of
        Left err -> throwError $ "Non-updatable status: " ++ err
        Right s  -> case invMap statusMap ^? ix s of
            Nothing    -> throwError $ "PlanMill doesn't know status " ++ show s
            Just enumS -> do
                -- update status
                let pm0 = pmUser pm
                let pm1 = pm0 { PM.uPassive = enumS }

                let pm' = pm1
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

    invMap = Map.fromList . map swap . Map.toList

-------------------------------------------------------------------------------
-- Contract Type
-------------------------------------------------------------------------------

canUpdateContractType :: P.Employee -> PMUser -> Either String Text
canUpdateContractType p pm = do
    -- this is cascade resembling `contractType'`.
    et <- maybe (Left "No employment type") Right $ p ^. P.employeeEmploymentType
    (ct, st) <-
        if et == P.External
        then return (P.FixedTerm, P.Hourly)
        else do
            ct <- maybe (Left "No contract type")   Right $ p ^. P.employeeContractType
            st <-
                if ct == P.PermanentAllIn
                then return P.Monthly
                else maybe (Left "No salary type")     Right $ p ^. P.employeeSalaryType
            return (ct, st)

    let pContractType = contractType' et ct st

    if pContractType == pmContract pm
    then Left $ "Contract types match: " ++ show pContractType
    else Right pContractType

contractType :: Maybe P.EmploymentType -> P.ContractType -> Maybe P.SalaryType -> Text
contractType et ct st = contractType'
    (fromMaybe P.Internal et)
    ct
    (fromMaybe P.Monthly st)

contractType' :: P.EmploymentType -> P.ContractType -> P.SalaryType -> Text
contractType' P.External _                _         = "Subcontractor"
contractType' P.Internal P.PermanentAllIn _         = "Employee all-in"
contractType' P.Internal _                P.Hourly  = "Employee (hourly)"
contractType' P.Internal _                P.Monthly = "Employee (monthly)"

updateContractType :: Ctx -> Login -> IO (CommandResponse ())
updateContractType ctx login = runLogT "update-contact-type" lgr $ runExceptT' $ do
    logInfo "Update contract type" login
    D _ p pm _ contractMap <- liftIO (fetchUser ctx login) >>= either throwError pure
    case canUpdateContractType p pm of
        Left err -> throwError $ "Non-updatable status: " ++ err
        Right s  -> case invMap contractMap ^? ix s of
            Nothing    -> throwError $ "PlanMill doesn't know contract type " ++ show s
            Just enumCT -> do
                -- update status
                let pm0 = pmUser pm
                let pm1 = pm0 { PM.uContractType = enumCT }

                let pm' = pm1
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

    invMap = Map.fromList . map swap . Map.toList

-------------------------------------------------------------------------------
-- Depart date
-------------------------------------------------------------------------------

-- | Return date for update
canUpdateDepartDate :: P.Employee -> PMUser -> Either String Day
canUpdateDepartDate p pmu = do
    d <- maybe (Left "No end date in Personio") Right $ p ^. P.employeeEndDate
    if Just d == PM.uDepartDate (pmUser pmu)
    then Left $ "Depart date set to " ++ show d
    else Right d

updateDepartDate :: Ctx -> Login -> IO (CommandResponse ())
updateDepartDate ctx login = runLogT "update-depart-date" lgr $ runExceptT' $ do
    logInfo "Add depart date" login
    D _ p pm _ _ <- liftIO (fetchUser ctx login) >>= either throwError pure
    case canUpdateDepartDate p pm of
        Left err -> throwError $ "Non-updatable depart date: " ++ err
        Right d  -> do
            -- update depart date
            let pm0 = pmUser pm
            let pm1 = pm0 { PM.uDepartDate = Just d }

            let pm' = pm1
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

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- TODO: move to futurice-servant?
runExceptT' :: Monad m => ExceptT String m a -> m (CommandResponse a)
runExceptT' m = do
    e <- runExceptT m
    return $ case e of
        Left err -> CommandResponseError err
        Right x  -> CommandResponseOk x

data D = D
    { dDay         :: Day
    , dPersonio    :: P.Employee
    , dPlanMill    :: PMUser
    , dStatusMap   :: Map (PM.EnumValue PM.User "passive") Text
    , dContractMap :: Map (PM.EnumValue PM.User "contractType") Text
    }

-- | Note: maybe we want to return PMUser (from .Types)
fetchUser :: Ctx -> Login -> IO (Either String D)
fetchUser ctx login = runExceptT $ do
    -- we submit to "write" workers, which always exist, though we read here
    -- (we are going to write!)
    epms <- liftIO $ PM.submitPlanMillE (ctxWriteWorkers ctx) PM.users
    pms <- case epms of
        Left err  -> throwError $ "Error fetching users from PlanMill:"  ++ show err
        Right pms -> return pms

    pmu <- case find (\pm -> pmLogin pm == Just login) pms of
        Nothing -> throwError "Cannot find planmill user"
        Just u  -> return u

    ExceptT $ runIntegrations' ctx $ runExceptT $ do
        ps <- lift $ P.personio P.PersonioEmployees
        p <- case find (\p -> p ^. P.employeeLogin == Just login) ps of
            Nothing -> throwError "Cannot find personio user"
            Just p  -> return p

        -- Here we don't care about competence
        u <- lift $ userToPMUser pmu pmu

        today <- currentDay
        statusMap <- lift $ PMQ.allEnumerationValues (Proxy :: Proxy PM.User) (Proxy :: Proxy "passive")
        contractMap <- lift $ PMQ.allEnumerationValues (Proxy :: Proxy PM.User) (Proxy :: Proxy "contractType")

        return D
            { dDay         = today
            , dPersonio    = p
            , dPlanMill    = u
            , dStatusMap   = statusMap
            , dContractMap = contractMap
            }

pmLogin :: PM.User -> Maybe Login
pmLogin u = match loginRe (PM.uUserName u)
  where
    loginRe = "https://login.futurice.com/openid/" *> loginRegexp
