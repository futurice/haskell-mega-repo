{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.Checklist.Types.TaskRole where

import Control.Lens      (Index, IxValue, Ixed (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep  (Representable (..), distributeRep, liftR2, pureRep)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

-- | States of the tasks
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

makePrisms ''TaskRole
deriveGeneric ''TaskRole
deriveLift ''TaskRole

instance TextEnum TaskRole where
    type TextEnumNames TaskRole =
        '["IT"
        , "HR"
        , "Supervisor"
        ]

taskRoleToText :: TaskRole -> Text
taskRoleToText = enumToText

taskRoleFromText :: Text -> Maybe TaskRole
taskRoleFromText = enumFromText

_TaskRole :: Prism' Text TaskRole
_TaskRole = enumPrism

deriveVia [t| Arbitrary TaskRole       `Via` Sopica TaskRole  |]
deriveVia [t| ToJSON TaskRole          `Via` Enumica TaskRole |]
deriveVia [t| FromJSON TaskRole        `Via` Enumica TaskRole |]
deriveVia [t| ToHttpApiData TaskRole   `Via` Enumica TaskRole |]
deriveVia [t| FromHttpApiData TaskRole `Via` Enumica TaskRole |]
deriveVia [t| Csv.ToField TaskRole     `Via` Enumica TaskRole |]
deriveVia [t| Csv.FromField TaskRole   `Via` Enumica TaskRole |]
deriveVia [t| ToHtml TaskRole          `Via` Enumica TaskRole |]

instance ToParamSchema TaskRole where toParamSchema = enumToParamSchema
instance ToSchema TaskRole where declareNamedSchema = enumDeclareNamedSchema

-------------------------------------------------------------------------------
-- PerTaskRole container
-------------------------------------------------------------------------------

data PerTaskRole a = PerTaskRole !a !a !a
  deriving (Functor)

instance NFData a => NFData (PerTaskRole a) where
    rnf (PerTaskRole x y z) =
        rnf x `seq` rnf y `seq` rnf z

type instance Index (PerTaskRole a) = TaskRole
type instance IxValue (PerTaskRole a) = a

instance Ixed (PerTaskRole a) where
    ix TaskRoleIT f (PerTaskRole x y z) = (\a -> PerTaskRole a y z) <$> f x
    ix TaskRoleHR f (PerTaskRole x y z) = (\a -> PerTaskRole x a z) <$> f y
    ix TaskRoleSupervisor f (PerTaskRole x y z) = PerTaskRole x y <$> f z

instance Semigroup a => Semigroup (PerTaskRole a) where
    (<>) = liftR2 (<>)

instance Monoid a => Monoid (PerTaskRole a) where
    mempty = pureRep mempty
    mappend = liftR2 mappend

instance Distributive PerTaskRole where
    distribute = distributeRep

instance Representable PerTaskRole where
    type Rep PerTaskRole = TaskRole

    tabulate f = PerTaskRole
        (f TaskRoleIT)
        (f TaskRoleHR)
        (f TaskRoleSupervisor)

    index (PerTaskRole x _ _) TaskRoleIT = x
    index (PerTaskRole _ x _) TaskRoleHR = x
    index (PerTaskRole _ _ x) TaskRoleSupervisor = x
