{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Contact (
    Contact(..),
    ContactId,
    Contacts,
    Phone(..),
    Postal(..),
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Account    (AccountId)
import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)

type ContactId = Identifier Contact
type Contacts = Vector Contact

data Phone = Phone Text
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable Phone
instance NFData Phone
instance AnsiPretty Phone
instance Binary Phone
instance Structured Phone

instance FromJSON Phone where
    parseJSON (String t) = return $ Phone t
    -- parseJSON (Number n) = return $ Phone "TODO"
    parseJSON _ = return $ Phone "" -- TODO: why empty phone?

data Postal = Postal Text
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable Postal
instance NFData Postal
instance AnsiPretty Postal
instance Binary Postal
instance Structured Postal

instance FromJSON Postal where
    parseJSON (String t) = return $ Postal t
    parseJSON _ = return $ Postal ""

data Contact = Contact
    { _cId                 :: !ContactId
    , cAccount             :: !(Maybe AccountId)
    , cAccountName         :: !(Maybe Text)
    , cAccountType         :: !(Maybe Int)
    , cCreated             :: !UTCTime
    , cCreatedBy           :: !(Maybe Text)
    , cDepartment          :: !(Maybe Text)
    , cEmail               :: !(Maybe Text)
    , cFax                 :: !(Maybe Text)
    , cGoogleTalk          :: !Phone
    , cInvoiceXsl          :: !(Maybe Text)
    , cLanguageCode        :: !(Maybe Text)
    , cLastName            :: !(Maybe Text)
    , cLinkedIn            :: !(Maybe Text)
    , cMobilePhone         :: !Phone
    , cModified            :: !(Maybe UTCTime)
    , cModifiedBy          :: !(Maybe Text)
    , cPassive             :: !(Maybe Int)
    , cPrimaryAddress      :: !(Maybe Text)
    , cPrimaryCity         :: !(Maybe Text)
    , cPrimaryCountry      :: !(Maybe Int)
    , cPrimaryPhone        :: !Phone
    , cPrimaryPostalCode   :: !Postal
    , cRole                :: !(Maybe Int)
    , cSecondaryAddress    :: !(Maybe Text)
    , cSecondaryCity       :: !(Maybe Text)
    , cSecondaryCountry    :: !(Maybe Int)
    , cSecondaryPostalCode :: !Postal
    , cSkype               :: !(Maybe Text)
    , cTitle               :: !(Maybe Text)
    , cWebsite             :: !(Maybe Text)
    , cWorkPhone           :: !Phone
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Contact
deriveGeneric ''Contact

instance HasIdentifier Contact Contact where
    identifier = cId

instance Hashable Contact
instance NFData Contact
instance AnsiPretty Contact
instance Binary Contact
instance Structured Contact

instance FromJSON Contact where
    parseJSON = withObject "Contact" $ \obj ->
        Contact <$> obj .: "id"
                <*> obj .: "account"
                <*> obj .: "accountName"
                <*> obj .: "accountType"
                <*> obj .: "created"
                <*> obj .: "createdBy"
                <*> obj .: "department"
                <*> obj .: "email"
                <*> optional (obj .: "fax")
                <*> obj .: "googleTalk"
                <*> obj .: "invoiceXsl"
                <*> obj .: "languageCode"
                <*> obj .: "lastName"
                <*> obj .: "linkedIn"
                <*> obj .: "mobilePhone"
                <*> optional (obj .: "modified")
                <*> obj .: "modifiedBy"
                <*> obj .: "passive"
                <*> obj .: "primaryAddress"
                <*> obj .: "primaryCity"
                <*> obj .: "primaryCountry"
                <*> obj .: "primaryPhone"
                <*> obj .: "primaryPostalCode"
                <*> obj .:? "role"
                <*> obj .: "secondaryAddress"
                <*> obj .: "secondaryCity"
                <*> obj .: "secondaryCountry"
                <*> obj .: "secondaryPostalCode"
                <*> obj .: "skype"
                <*> obj .: "title"
                <*> obj .: "website"
                <*> obj .: "workPhone"

deriveGeneric ''Phone
deriveGeneric ''Postal
