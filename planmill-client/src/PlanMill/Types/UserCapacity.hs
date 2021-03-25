{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.UserCapacity (
    UserCapacity(..),
    UserCapacities,
    ) where

import PlanMill.Internal.Prelude

type UserCapacities = Vector UserCapacity

data UserCapacity = UserCapacity
    { userCapacityDate        :: !Day
    , userCapacityAmount      :: !(NDT 'Minutes Int)
    , userCapacityDescription :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''UserCapacity

instance Hashable UserCapacity
instance NFData UserCapacity
instance AnsiPretty UserCapacity
instance Binary UserCapacity
instance Structured UserCapacity

instance FromJSON UserCapacity where
    parseJSON = withObject "UserCapacity" $ \obj -> UserCapacity
        <$> (dayFromZ <$> obj .: "date")
        <*> obj .: "amount"
        <*> obj .: "description"
