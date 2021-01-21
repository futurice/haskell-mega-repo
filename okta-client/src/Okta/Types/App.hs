{-# LANGUAGE DeriveLift #-}
module Okta.Types.App where

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Text as T

newtype OktaAppId = OktaAppId Text
    deriving (Eq, Show, Generic, Lift)
    deriving anyclass (Hashable, NFData, AnsiPretty)
    deriving newtype (FromJSON, ToJSON)

instance FromEnvVar OktaAppId where
    fromEnvVar = Just . OktaAppId . T.pack
