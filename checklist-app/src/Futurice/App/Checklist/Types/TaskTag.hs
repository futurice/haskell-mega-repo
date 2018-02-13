{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskTag where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Lucid.Foundation (ToHtml (..))
import Futurice.Prelude
import Prelude ()

data TaskTag
    = GithubTask    -- ^ This task relates to Github
    | PlanmillTask  -- ^ This task relates to Planmill
 deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

makePrisms ''TaskTag
deriveGeneric ''TaskTag
deriveLift ''TaskTag

ei :: EnumInstances TaskTag
ei = sopEnumInstances $
    K "github" :*
    K "planmill" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

taskTagToText :: TaskTag -> Text
taskTagToText = enumToText ei

taskTagFromText :: Text -> Maybe TaskTag
taskTagFromText = enumFromText ei

_TaskTag :: Prism' Text TaskTag
_TaskTag = enumPrism ei

instance NFData TaskTag

instance Arbitrary TaskTag where
    arbitrary = sopArbitrary
    shrink    = sopShrink

-- This isn't copy paste
instance ToHtml TaskTag where
    toHtmlRaw = toHtml
    toHtml GithubTask   = "GitHub"
    toHtml PlanmillTask = "PlanMill"

instance ToParamSchema TaskTag where
    toParamSchema = enumToParamSchema ei

instance ToSchema TaskTag where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON TaskTag where
    toJSON = enumToJSON ei

instance FromJSON TaskTag where
    parseJSON = enumParseJSON ei

instance FromHttpApiData TaskTag where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData TaskTag where
    toUrlPiece = enumToUrlPiece ei
