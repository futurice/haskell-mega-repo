{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Me (Me(..)) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier (HasIdentifier (..))
import PlanMill.Types.User       (User, UserId)

data Me = Me
    { meFirstName :: !Text
    , meLastName  :: !Text
    , _meUid      :: !UserId
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Me
deriveGeneric ''Me

instance HasIdentifier Me User where
    identifier = meUid

instance Hashable Me
instance NFData Me
instance AnsiPretty Me
instance Binary Me
instance Structured Me

instance FromJSON Me where
    parseJSON = withObject "Me" $ \obj ->
        Me <$> obj .: "firstName"
           <*> obj .: "lastName"
           <*> obj .: "id"
