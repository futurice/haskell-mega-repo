{-# LANGUAGE DeriveLift #-}
module Okta.Types.Group where

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Text as T

newtype OktaGroupId = OktaGroupId Text
    deriving (Eq, Show, Generic, Lift)
    deriving anyclass (Hashable)
    deriving newtype (FromJSON, ToJSON)

instance FromEnvVar OktaGroupId where
    fromEnvVar = Just . OktaGroupId . T.pack
