{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -freduction-depth=61 #-}
module Futurice.App.Contacts.Types (
    ContactFD(..),
    ContactGH(..),
    Contact(..),
    ) where

import Data.Csv          (ToField (..))
import FUM.Types.Login   (Login)
import Futurice.Company
import Futurice.Generics
import Futurice.IsMaybe
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Personio            as P

-------------------------------------------------------------------------------
-- Flowdock
-------------------------------------------------------------------------------

data ContactFD avatar = ContactFD
    { cfdId     :: !Int       -- ^ Identifier
    , cfdNick   :: !Text      -- ^ Nick
    , cfdAvatar :: !avatar    -- ^ Avatar
    }
  deriving stock
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )
  deriving anyclass (NFData)

deriveGeneric ''ContactFD

deriveVia [t| forall a. ((ToJSON a, IsMaybe a)   => ToJSON (ContactFD a))   `Via` Sopica (ContactFD a) |]
deriveVia [t| forall a. ((FromJSON a, IsMaybe a) => FromJSON (ContactFD a)) `Via` Sopica (ContactFD a) |]

instance ToSchema a => ToSchema (ContactFD a) where
    declareNamedSchema = sopDeclareNamedSchema

instance ToField (ContactFD a) where
    toField = toField . cfdNick

-------------------------------------------------------------------------------
-- Github
-------------------------------------------------------------------------------

data ContactGH avatar = ContactGH
    { cghNick   :: !Text
    , cghAvatar :: !avatar
    }
  deriving stock
    ( Eq, Ord, Show, Read, Generic, Typeable
    , Functor, Foldable, Traversable
    )
  deriving anyclass (NFData)

deriveGeneric ''ContactGH

deriveVia [t| forall a. ((ToJSON a, IsMaybe a)   => ToJSON (ContactGH a))   `Via` Sopica (ContactGH a) |]
deriveVia [t| forall a. ((FromJSON a, IsMaybe a) => FromJSON (ContactGH a)) `Via` Sopica (ContactGH a) |]

instance ToSchema a => ToSchema (ContactGH a) where
    declareNamedSchema = sopDeclareNamedSchema
instance ToField (ContactGH a) where
    toField = toField . cghNick

-------------------------------------------------------------------------------
-- Contact
-------------------------------------------------------------------------------

data Contact avatar = Contact
    { contactLogin      :: !Login
    , contactFirst      :: !Text
    , contactName       :: !Text
    , contactEmail      :: !Text
    , contactPhones     :: ![Text]
    , contactTitle      :: !(Maybe Text)
    , contactThumb      :: !avatar
    , contactImage      :: !Text
    , contactFlowdock   :: !(Maybe (ContactFD avatar))
    , contactGithub     :: !(Maybe (ContactGH avatar))
    , contactTeam       :: !Tribe
    , contactOffice     :: !Office
    , contactEmployer   :: !Company
    , contactCountry    :: !Country
    , contactCompetence :: !Text
    , contactExternal   :: !Bool
    , contactHrnumber   :: !(Maybe Int)
    , contactPersonio   :: !P.EmployeeId
    }
  deriving stock
    ( Eq, Ord, Show, Generic, Typeable
    , Functor, Foldable, Traversable
    )
  deriving anyclass (NFData)

-- TH slices

deriveGeneric ''Contact

deriveVia [t| forall a. ((ToJSON a, IsMaybe a)   => ToJSON (Contact a))   `Via` Sopica (Contact a) |]
deriveVia [t| forall a. ((FromJSON a, IsMaybe a) => FromJSON (Contact a)) `Via` Sopica (Contact a) |]
deriveVia [t| forall a. DefaultOrdered (Contact a) `Via` Sopica (Contact a) |]

instance ToSchema a => ToSchema (Contact a) where
    declareNamedSchema = sopDeclareNamedSchema

instance ToField a => ToNamedRecord (Contact a) where
    toNamedRecord Contact {..} = HM.fromList
        [ (,) "login"      $ toField contactLogin
        , (,) "first"      $ toField contactFirst
        , (,) "name"       $ toField contactName
        , (,) "email"      $ toField contactEmail
        , (,) "phones"     $ toField $ listToMaybe contactPhones
        , (,) "title"      $ toField contactTitle
        , (,) "thumb"      $ toField contactThumb
        , (,) "image"      $ toField contactImage
        , (,) "flowdock"   $ toField contactFlowdock
        , (,) "github"     $ toField contactGithub
        , (,) "team"       $ toField contactTeam
        , (,) "competence" $ toField contactCompetence
        , (,) "hrnumber"   $ toField contactHrnumber
        , (,) "personio"   $ toField contactPersonio
        ]
