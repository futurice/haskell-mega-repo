{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.Types where

import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Futurice.Generics
import Data.Char (isLower)
import Algebra.Lattice ((\/))
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
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData)

instance Textual PolicyName where
    textualToText   = coerce
    textualFromText t
        | T.all (isLower \/ (== '-')) t = Right (PolicyName t)
        | otherwise = Left "Policy name should consist only from lower case letter and dash (-)"

deriveVia [t| ToJSON PolicyName             `Via` Textica PolicyName |]
deriveVia [t| FromJSON PolicyName           `Via` Textica PolicyName |]
deriveVia [t| ToHttpApiData PolicyName      `Via` Textica PolicyName |]
deriveVia [t| FromHttpApiData PolicyName    `Via` Textica PolicyName |]
deriveVia [t| Postgres.ToField PolicyName   `Via` Textica PolicyName |]
deriveVia [t| Postgres.FromField PolicyName `Via` Textica PolicyName |]
deriveVia [t| ToHtml PolicyName             `Via` Textica PolicyName |]

instance ToParamSchema PolicyName where toParamSchema = textualToParamSchema
instance ToSchema PolicyName where declareNamedSchema = textualDeclareNamedSchema

-------------------------------------------------------------------------------
-- UserName
-------------------------------------------------------------------------------

newtype UserName = UserName Text
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData)

instance Textual UserName where
    textualToText   = coerce
    textualFromText t
        | T.all (isLower \/ (== '-')) t = Right (UserName t)
        | otherwise = Left "User name should consist only from lower case letter and dash (-)"

deriveVia [t| ToJSON UserName             `Via` Textica UserName |]
deriveVia [t| FromJSON UserName           `Via` Textica UserName |]
deriveVia [t| ToHttpApiData UserName      `Via` Textica UserName |]
deriveVia [t| FromHttpApiData UserName    `Via` Textica UserName |]
deriveVia [t| Postgres.ToField UserName   `Via` Textica UserName |]
deriveVia [t| Postgres.FromField UserName `Via` Textica UserName |]
deriveVia [t| ToHtml UserName             `Via` Textica UserName |]

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
