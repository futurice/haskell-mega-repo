{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.FUM.Ctx (
    Ctx (..),
    newCtx,
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Pool              (Pool, createPool)
import Futurice.CryptoRandom  (CryptoGen, mkCryptoGen)
import Futurice.IdMap         (IdMap)
import Futurice.Prelude
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Personio

import Futurice.App.FUM.Types


data Ctx = Ctx
    { ctxLogger              :: !Logger
    , ctxMockUser            :: !(Maybe Login)
    , ctxPersonio            :: !(TVar (IdMap Personio.Employee))
    , ctxPersonioValidations :: !(TVar [Personio.EmployeeValidation])
    , ctxWorld               :: !(TVar World)
    , ctxPostgres            :: !(Pool Postgres.Connection)
    , ctxPRNGs               :: !(Pool (TVar CryptoGen))
    }

newCtx
    :: Logger
    -> Maybe Login
    -> Postgres.ConnectInfo
    -> IdMap Personio.Employee
    -> [Personio.EmployeeValidation]
    -> World
    -> IO Ctx
newCtx logger mockUser ci es vs w = Ctx logger mockUser
    <$> newTVarIO es
    <*> newTVarIO vs
    <*> newTVarIO w
    <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
    <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
