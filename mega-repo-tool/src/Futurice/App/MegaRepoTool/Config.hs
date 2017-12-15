{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.MegaRepoTool.Config where

import Data.Aeson
       (FromJSON (..), withObject, withText, (.:))
import Futurice.Prelude
import Prelude ()

import qualified Text.Microstache as M

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type AppName = Text
type DebName = Text

newtype MustacheT = MustacheT M.Template
  deriving (Show)

instance FromJSON MustacheT where
    parseJSON = withText "Mustache template"
        $ either (fail . show) (pure . MustacheT)
        . M.compileMustacheText "<input>" . view lazy

data ImageDefinition = ImageDefinition
    { _idDockerImage :: !Text
    , _idExecutable  :: !Text
    }
  deriving (Show)

instance FromJSON ImageDefinition where
    parseJSON = withObject "ImageDefinition" $ \obj -> ImageDefinition
        <$> obj .: "docker"
        <*> obj .: "executable"

data MRTConfig = MRTConfig
    { mrtDockerBaseImage :: !Text
    , _mrtApps           :: !(Map AppName ImageDefinition)
    , mtrDebs           :: [DebName]
    , mrtDockerfileTmpl :: MustacheT
    }
  deriving (Show)

makeLenses ''MRTConfig

instance FromJSON MRTConfig where
    parseJSON = withObject "MRTConfig" $ \obj -> MRTConfig
        <$> obj .: "docker-base-image"
        <*> obj .: "apps"
        <*> obj .: "debs"
        <*> obj .: "dockerfile-template"
