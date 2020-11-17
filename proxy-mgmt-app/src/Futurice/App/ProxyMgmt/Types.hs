{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.Types where

import Algebra.Lattice                    ((\/))
import Data.Char                          (isLower)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API (LenientEndpoint)

import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

-------------------------------------------------------------------------------
-- PolicyName
-------------------------------------------------------------------------------

newtype PolicyName = PolicyName Text
  deriving stock (Eq, Ord, GhcGeneric)
  deriving newtype (Show, NFData)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Postgres.ToField, Postgres.FromField, ToHtml) via (Textica PolicyName)

instance Textual PolicyName where
    textualToText   = coerce
    textualFromText t
        | T.all (isLower \/ (== '-')) t = Right (PolicyName t)
        | otherwise = Left "Policy name should consist only from lower case letter and dash (-)"

instance ToParamSchema PolicyName where toParamSchema = textualToParamSchema
instance ToSchema PolicyName where declareNamedSchema = textualDeclareNamedSchema

-------------------------------------------------------------------------------
-- UserName
-------------------------------------------------------------------------------

newtype UserName = UserName Text
  deriving stock (Eq, Ord, GhcGeneric)
  deriving newtype (Show, NFData)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Postgres.ToField, Postgres.FromField, ToHtml) via (Textica UserName)

instance Textual UserName where
    textualToText   = coerce
    textualFromText t
        | T.all (isLower \/ (== '-')) t = Right (UserName t)
        | otherwise = Left "User name should consist only from lower case letter and dash (-)"

instance ToParamSchema UserName where toParamSchema = textualToParamSchema
instance ToSchema UserName where declareNamedSchema = textualDeclareNamedSchema

-------------------------------------------------------------------------------
-- Token
-------------------------------------------------------------------------------

data Token = Token
    { tUserName   :: !UserName
    , tActive     :: !Bool
    , tUsertype   :: !Text
    , tPolicyName :: !PolicyName
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

instance FromRow Token where
    fromRow = Token <$> field <*> field <*> field <*> field

-------------------------------------------------------------------------------
-- AccessEntry
-------------------------------------------------------------------------------

data AccessEntry = AccessEntry
    { aeUser     :: !UserName
    , aeStamp    :: !UTCTime
    , aeEndpoint :: !LenientEndpoint
    }
  deriving (Show, Generic)

instance NFData AccessEntry
instance FromRow AccessEntry where
    fromRow = AccessEntry <$> field <*> field <*> field
