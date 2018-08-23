{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Personio.Eval (
    evalPersonioReq,
    ) where

import Control.Monad.Http
import Data.Aeson.Compat  (FromJSON (..), decode)
import Data.Aeson.Types   (listParser)
import Futurice.Clock
import Futurice.Prelude
import Personio.Request
import Personio.Types
import Prelude ()

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Network.HTTP.Client as H

evalPersonioReq
    :: ( MonadHttp m, MonadThrow m
       , MonadLog m, MonadClock m, MonadTime m
       , MonadReader env m, HasPersonioCfg env
       )
    => PersonioReq a
    -> m a
evalPersonioReq personioReq = do
    Cfg (BaseUrl baseUrl) (ClientId clientId) (ClientSecret clientSecret) <-
        view personioCfg

    -- Get access token
    let tokenUrl = baseUrl <> "/v1/auth?client_id=" <> clientId <> "&" <> "client_secret=" <> clientSecret
    tokenReq <- H.parseUrlThrow (tokenUrl ^. unpacked)
    logTrace_ "personio token request"
    (tokenDur, tokenRes) <- clocked $ httpLbs tokenReq { H.method = "POST" }
    logTrace "personio access token" (WrapResponse tokenRes)
    logTrace "personio access token duration" tokenDur
    Envelope (AccessToken token) <- decode (H.responseBody tokenRes)

    -- Perform request
    let url = (baseUrl <> "/v1/company/employees") ^. unpacked
    bs <- personioHttpLbs token url

    case personioReq of
        PersonioEmployees -> do
            Envelope (E employees) <- decode bs
            pure employees
        PersonioValidations -> do
            -- We ask for employees, but parse them differently
            Envelope (V validations) <- decode bs
            pure validations
        PersonioAll -> do
            Envelope (E employees) <- decode bs
            Envelope (V validations) <- decode bs
            pure (employees, validations)
        -- Direct access doesn't know this.
        -- TODO: maybe return current.
        PersonioActive -> do
            Envelope (E employees) <- decode bs
            today <- currentDay
            pure $ Map.singleton today $ Set.fromList
                [ e ^. employeeId
                | e <- employees
                , employeeIsActive today e
                ]
  where
    personioHttpLbs token url = do
        req <- H.parseUrlThrow url
        logTrace "personio request" url
        (dur, res) <- clocked $ httpLbs req
            { H.requestHeaders
                = ("Authorization", encodeUtf8 $ "Bearer " <> token)
                : H.requestHeaders req
            }
        logTrace "personio response" dur
        -- logTrace "response" (T.take 10000 $ decodeUtf8Lenient $ H.responseBody res ^. strict)
        pure (H.responseBody res)


-- | A wrapper around list of 'Employee's, using
-- 'parsePersonioEmployee' in 'FromJSON' instance.
newtype E = E [Employee]

instance FromJSON E where
    parseJSON = fmap E . listParser parsePersonioEmployee


newtype V = V [EmployeeValidation]

instance FromJSON V where
    parseJSON
      = fmap (V . postValidatePersonioEmployees)
      . listParser validatePersonioEmployee
