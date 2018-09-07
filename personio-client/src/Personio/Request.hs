{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Personio.Request (
    PersonioReq(..),
    requestDict,
    -- * All data
    PersonioAllData (..),
    -- * Some
    SomePersonioReq (..),
    SomePersonioRes (..),
    ) where

import Data.Aeson.Compat    (object, pairs, withObject, withText, (.:), (.=))
import Data.Constraint      (Dict (..))
import Futurice.CareerLevel
import Futurice.Generics
import Futurice.Prelude
import Personio.Types
import Prelude ()

import qualified Data.Swagger as Swagger

data PersonioAllData = PersonioAllData
    { paEmployees        :: ![Employee]
    , paValidations      :: ![EmployeeValidation]
    , paCareerLevels     :: !(Map CareerLevel Int)
    , paCareerLevelsRole :: !(Map Text (Map CareerLevel Int))
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PersonioReq a where
    PersonioEmployees       :: PersonioReq [Employee]
    PersonioValidations     :: PersonioReq [EmployeeValidation]
    PersonioAll             :: PersonioReq PersonioAllData
    PersonioSimpleEmployees :: PersonioReq (Map Day [SimpleEmployee])

deriving instance Eq (PersonioReq a)
deriving instance Ord (PersonioReq a)
deriving instance Show (PersonioReq a)

instance Hashable (PersonioReq a) where
    hashWithSalt salt PersonioEmployees =
        salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt PersonioValidations =
        salt `hashWithSalt` (1 :: Int)
    hashWithSalt salt PersonioAll =
        salt `hashWithSalt` (2 :: Int)
    hashWithSalt salt PersonioSimpleEmployees =
        salt `hashWithSalt` (3 :: Int)

requestDict
    :: ( c [Employee], c [EmployeeValidation]
       , c PersonioAllData
       , c (Map Day [SimpleEmployee])
       )
    => Proxy c
    -> PersonioReq a
    -> Dict (c a)
requestDict _ PersonioEmployees   = Dict
requestDict _ PersonioValidations = Dict
requestDict _ PersonioAll         = Dict
requestDict _ PersonioSimpleEmployees      = Dict

-------------------------------------------------------------------------------
-- Some
-------------------------------------------------------------------------------

-- | Request without tag
data SomePersonioReq where
    SomePersonioReq :: PersonioReq a -> SomePersonioReq

instance ToJSON SomePersonioReq where
    toJSON (SomePersonioReq PersonioEmployees)       = "employees"
    toJSON (SomePersonioReq PersonioValidations)     = "validations"
    toJSON (SomePersonioReq PersonioAll)             = "all"
    toJSON (SomePersonioReq PersonioSimpleEmployees) = "simple-employees"

instance FromJSON SomePersonioReq where
    parseJSON = withText "PersonioReq" $ \t -> case t of
        "employees"        -> pure $ SomePersonioReq PersonioEmployees
        "validations"      -> pure $ SomePersonioReq PersonioValidations
        "all"              -> pure $ SomePersonioReq PersonioAll
        "simple-employees" -> pure $ SomePersonioReq PersonioSimpleEmployees
        _             -> fail $ "Invalid PersonioReq " ++ show t

instance ToParamSchema SomePersonioReq where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        & Swagger.enum_ ?~
            [ toJSON $ SomePersonioReq PersonioEmployees
            , toJSON $ SomePersonioReq PersonioValidations
            , toJSON $ SomePersonioReq PersonioAll
            , toJSON $ SomePersonioReq PersonioSimpleEmployees
            ]

instance ToSchema SomePersonioReq where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "SomePersonioReq") $
        Swagger.paramSchemaToSchema p

-- | Response, we include the request as then we can refine the type.
data SomePersonioRes where
    SomePersonioRes :: PersonioReq a -> a -> SomePersonioRes

instance ToJSON SomePersonioRes where
    toJSON (SomePersonioRes req res) = object
        [ "req" .= SomePersonioReq req
        , case requestDict (Proxy :: Proxy ToJSON) req of
            Dict -> "res" .= res
        ]

    toEncoding (SomePersonioRes req res) = pairs $ mconcat
        [ "req" .= SomePersonioReq req
        , case requestDict (Proxy :: Proxy ToJSON) req of
            Dict -> "res" .= res
        ]

instance FromJSON SomePersonioRes where
    parseJSON = withObject "PersonioRes" $ \obj -> do
        SomePersonioReq req <- obj .: "req"
        case requestDict (Proxy :: Proxy FromJSON) req of
            Dict -> do
                res <- obj .: "res"
                pure (SomePersonioRes req res)

instance ToSchema SomePersonioRes where
    declareNamedSchema _ =
        pure $ Swagger.NamedSchema (Just "SomePersonioRes") mempty
