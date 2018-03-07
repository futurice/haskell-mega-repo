{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.Checklist.Types.TaskTag (
    TaskTag (..),
    -- * Prisms
    _TaskTag,
    _GithubTask,
    _PlanmillTask,
    -- * Conversion functions
    taskTagToText,
    taskTagFromText,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data TaskTag
    = GithubTask       -- ^ This task relates to Github
    | PlanmillTask     -- ^ This task relates to Planmill
    | FirstContactTask -- ^ This task relates to accepting job offer
 deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

makePrisms ''TaskTag
deriveGeneric ''TaskTag
deriveLift ''TaskTag

instance TextEnum TaskTag where
    type TextEnumNames TaskTag = '["GitHub", "PlanMill", "FirstContact"]

taskTagToText :: TaskTag -> Text
taskTagToText = enumToText

taskTagFromText :: Text -> Maybe TaskTag
taskTagFromText = enumFromText

_TaskTag :: Prism' Text TaskTag
_TaskTag = enumPrism

deriveVia [t| Arbitrary TaskTag       `Via` Sopica TaskTag  |]
deriveVia [t| ToJSON TaskTag          `Via` Enumica TaskTag |]
deriveVia [t| FromJSON TaskTag        `Via` Enumica TaskTag |]
deriveVia [t| ToHttpApiData TaskTag   `Via` Enumica TaskTag |]
deriveVia [t| FromHttpApiData TaskTag `Via` Enumica TaskTag |]
deriveVia [t| Csv.ToField TaskTag     `Via` Enumica TaskTag |]
deriveVia [t| Csv.FromField TaskTag   `Via` Enumica TaskTag |]
deriveVia [t| ToHtml TaskTag          `Via` Enumica TaskTag |]

instance ToParamSchema TaskTag where toParamSchema = enumToParamSchema
instance ToSchema TaskTag where declareNamedSchema = enumDeclareNamedSchema
