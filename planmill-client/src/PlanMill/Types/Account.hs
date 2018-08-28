-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Account (Account(..), Accounts, AccountId) where

-- Account is defined in 'User' because they form a loop.
import PlanMill.Types.User (Account (..), Accounts, AccountId)
