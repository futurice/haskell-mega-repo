{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2017 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Vacation (
    Vacation(..),
    Vacations,
    VacationId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)

type VacationId = Identifier Vacation
type Vacations = Vector Vacation

data Vacation = Vacation
    { _vacationId           :: !VacationId
    , vacationDaysRemaining :: !(NDT 'Minutes Int)
    , vacationyear          :: !Int
    }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Vacation
deriveGeneric ''Vacation

instance HasIdentifier Vacation Vacation where
    identifier = vacationId

instance Hashable Vacation
instance NFData Vacation
instance AnsiPretty Vacation
instance Binary Vacation
instance Structured Vacation

instance FromJSON Vacation where
    parseJSON = withObject "Vacation" $ \obj -> Vacation
        <$> obj .: "id"
        <*> obj .: "daysRemaining"
        <*> obj .: "vacationYear"
