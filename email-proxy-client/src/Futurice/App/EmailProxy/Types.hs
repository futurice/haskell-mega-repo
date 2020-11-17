{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Futurice.App.EmailProxy.Types where

import Data.Aeson        (withText)
import Futurice.Email
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.List.NonEmpty as NE

-------------------------------------------------------------------------------
-- Email address
-------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress { getEmailAddress :: Text }
  deriving stock (Show)
  deriving newtype (ToJSON)

fromEmail :: Email -> EmailAddress
fromEmail = EmailAddress . emailToText

makePrisms ''EmailAddress
deriveGeneric ''EmailAddress

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

emptyReq :: EmailAddress -> Req
emptyReq x = Req (pure x) Nothing Nothing Nothing mempty mempty

-- TODO: move to futurice-prelude or even lens.
nonEmpty :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
nonEmpty = iso NE.nonEmpty (maybe [] NE.toList)

makeLenses ''Req
deriveGeneric ''Req

deriveVia [t| ToJSON Req   `Via` Sopica Req |]
deriveVia [t| FromJSON Req `Via` Sopica Req |]

instance ToSchema Req where declareNamedSchema = sopDeclareNamedSchema
