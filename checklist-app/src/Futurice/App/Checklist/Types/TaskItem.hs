{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.Checklist.Types.TaskItem where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv        as Csv
import qualified FUM.Types.Login as FUM

-------------------------------------------------------------------------------
-- TaskItem
-------------------------------------------------------------------------------

-- | States of the tasks
data TaskItem
    = TaskItemDone
    | TaskItemTodo
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''TaskItem
deriveGeneric ''TaskItem

instance TextEnum TaskItem where
    type TextEnumNames TaskItem = '["done", "todo" ]

taskItemToText :: TaskItem -> Text
taskItemToText = enumToText

taskItemFromText :: Text -> Maybe TaskItem
taskItemFromText = enumFromText

_TaskItem :: Prism' Text TaskItem
_TaskItem = enumPrism

deriveVia [t| Arbitrary TaskItem       `Via` Sopica TaskItem  |]
deriveVia [t| ToJSON TaskItem          `Via` Enumica TaskItem |]
deriveVia [t| FromJSON TaskItem        `Via` Enumica TaskItem |]
deriveVia [t| ToHttpApiData TaskItem   `Via` Enumica TaskItem |]
deriveVia [t| FromHttpApiData TaskItem `Via` Enumica TaskItem |]
deriveVia [t| Csv.ToField TaskItem     `Via` Enumica TaskItem |]
deriveVia [t| Csv.FromField TaskItem   `Via` Enumica TaskItem |]
deriveVia [t| ToHtml TaskItem          `Via` Enumica TaskItem |]

instance ToParamSchema TaskItem where toParamSchema = enumToParamSchema
instance ToSchema TaskItem where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- AnnTaskItem
-------------------------------------------------------------------------------

-- | Annotated task item with who and when have done it.
data AnnTaskItem
    = AnnTaskItemDone !Text !FUM.Login !UTCTime
    | AnnTaskItemTodo !Text
  deriving (Eq, Ord, Show, Typeable, Generic)

annTaskItemTodo :: AnnTaskItem
annTaskItemTodo = AnnTaskItemTodo ""

annTaskItemComment :: Lens' AnnTaskItem Text
annTaskItemComment = lens g s
  where
    g (AnnTaskItemDone t _ _) = t
    g (AnnTaskItemTodo t)     = t
    s (AnnTaskItemDone _ x y) t = AnnTaskItemDone t x y
    s (AnnTaskItemTodo _)     t = AnnTaskItemTodo t

makePrisms ''AnnTaskItem
