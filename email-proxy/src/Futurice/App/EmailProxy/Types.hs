{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.EmailProxy.Types where

import Prelude ()
import Data.Aeson (withText)
import Futurice.Prelude
import Futurice.Generics

-------------------------------------------------------------------------------
-- Email address
-------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress { getEmailAddress :: Text }
  deriving (Show)

makePrisms ''EmailAddress
deriveGeneric ''EmailAddress

instance ToJSON EmailAddress where
    toJSON     = toJSON . getEmailAddress
    toEncoding = toEncoding . getEmailAddress

instance FromJSON EmailAddress where
    parseJSON = withText "Email address" $ pure . EmailAddress

instance ToSchema EmailAddress where
    declareNamedSchema = newtypeDeclareNamedSchema

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

data Req = Req
    { _reqTo      :: !(NonEmpty EmailAddress)
    , _reqCc      :: !(Maybe (NonEmpty EmailAddress)) -- maybe to make generic derivation work as we want it to.
    , _reqBcc     :: !(Maybe (NonEmpty EmailAddress))
    , _reqReplyTo :: !(Maybe EmailAddress)
    , _reqSubject :: !Text
    , _reqBody    :: !Text
    }
  deriving (Show)

makeLenses ''Req
deriveGeneric ''Req

instance ToJSON Req where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance FromJSON Req where
    parseJSON = sopParseJSON

instance ToSchema Req where
    declareNamedSchema = sopDeclareNamedSchema
