{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.Checklist.Types.ChecklistId (
    ChecklistId (..),
    -- * Prisms
    _ChecklistId,
    _NewEmployeeChecklist,
    _LeavingEmployeeChecklist,
    -- * Conversion functions
    checklistIdToText,
    checklistIdFromText,
    -- * PerChecklist
    PerChecklist (..),
    perNewEmployee,
    perLeavingEmployee,
    perFromInternalToExternal,
    perFromFuturiceToSubsidiary,
    ) where

import Control.Lens      (Index, IxValue, Ixed (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep  (Representable (..), distributeRep, liftR2, pureRep)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data ChecklistId
    = NewEmployeeChecklist
    | LeavingEmployeeChecklist
    | FromInternalToExternalChecklist
    | FromFuturiceToSubsidiaryChecklist
 deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic, Lift)
 deriving anyclass (NFData, Hashable)

makePrisms ''ChecklistId
deriveGeneric ''ChecklistId

instance TextEnum ChecklistId where
    type TextEnumNames ChecklistId =
        '["new-employee"
        , "leaving-employee"
        , "from-internal-to-external"
        , "from-futurice-to-subsidiary"
        ]

checklistIdToText :: ChecklistId -> Text
checklistIdToText = enumToText

checklistIdFromText :: Text -> Maybe ChecklistId
checklistIdFromText = enumFromText

_ChecklistId :: Prism' Text ChecklistId
_ChecklistId = enumPrism

deriveVia [t| Arbitrary ChecklistId       `Via` Sopica ChecklistId  |]
deriveVia [t| ToJSON ChecklistId          `Via` Enumica ChecklistId |]
deriveVia [t| FromJSON ChecklistId        `Via` Enumica ChecklistId |]
deriveVia [t| ToHttpApiData ChecklistId   `Via` Enumica ChecklistId |]
deriveVia [t| FromHttpApiData ChecklistId `Via` Enumica ChecklistId |]
deriveVia [t| Csv.ToField ChecklistId     `Via` Enumica ChecklistId |]
deriveVia [t| Csv.FromField ChecklistId   `Via` Enumica ChecklistId |]
deriveVia [t| ToHtml ChecklistId          `Via` Enumica ChecklistId |]

instance ToParamSchema ChecklistId where toParamSchema = enumToParamSchema
instance ToSchema ChecklistId where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- Per
-------------------------------------------------------------------------------

data PerChecklist a = PerChecklist
    { _perNewEmployee              :: !a
    , _perLeavingEmployee          :: !a
    , _perFromInternalToExternal   :: !a
    , _perFromFuturiceToSubsidiary :: !a
    }
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

makeLenses ''PerChecklist

type instance Index (PerChecklist a) = ChecklistId
type instance IxValue (PerChecklist a) = a

instance Ixed (PerChecklist a) where
    ix = pick

instance Pick (PerChecklist a) where
    pick NewEmployeeChecklist f              = perNewEmployee f
    pick LeavingEmployeeChecklist f          = perLeavingEmployee f
    pick FromInternalToExternalChecklist f   = perFromInternalToExternal f
    pick FromFuturiceToSubsidiaryChecklist f = perFromFuturiceToSubsidiary f

instance Semigroup a => Semigroup (PerChecklist a) where
    (<>) = liftR2 (<>)

instance Monoid a => Monoid (PerChecklist a) where
    mempty = pureRep mempty
    mappend = liftR2 mappend

instance Distributive PerChecklist where
    distribute = distributeRep

instance Representable PerChecklist where
    type Rep PerChecklist = ChecklistId

    tabulate f = PerChecklist
        { _perNewEmployee     = f NewEmployeeChecklist
        , _perLeavingEmployee = f LeavingEmployeeChecklist
        , _perFromInternalToExternal = f FromInternalToExternalChecklist
        , _perFromFuturiceToSubsidiary = f FromFuturiceToSubsidiaryChecklist
        }

    index xs NewEmployeeChecklist              = _perNewEmployee xs
    index xs LeavingEmployeeChecklist          = _perLeavingEmployee xs
    index xs FromInternalToExternalChecklist   = _perFromInternalToExternal xs
    index xs FromFuturiceToSubsidiaryChecklist = _perFromFuturiceToSubsidiary xs
