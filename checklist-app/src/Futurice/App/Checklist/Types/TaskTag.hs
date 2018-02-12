{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskTag where

import Data.Aeson.Compat (Value (String), withText)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Map  as Map
import qualified Data.Text as T

data TaskTag
    = GithubTask
      -- ^ This task relates to Github
    | PlanmillTask
      -- ^ This task relates to Planmill
 deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

makePrisms ''TaskTag
deriveGeneric ''TaskTag

_TaskTag :: Prism' Text TaskTag
_TaskTag = prism' taskTagToText taskTagFromText

taskTagToText :: TaskTag -> Text
taskTagToText GithubTask  = "github"
taskTagToText PlanmillTask = "planmill"

taskTagFromText :: Text -> Maybe TaskTag
taskTagFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ taskTagToText x, x)) [minBound .. maxBound]

instance Arbitrary TaskTag where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON TaskTag where
    toJSON = String . taskTagToText

instance FromJSON TaskTag where
    parseJSON = withText "TaskTag" $ \t ->
      maybe (fail $ "invalid taskRole " <> t ^. unpacked) pure $ t ^? _TaskTag
