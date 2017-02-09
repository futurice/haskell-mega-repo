{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | TODO: this should be doctested, but it isn't.
module Futurice.Monisto.Tutorial (
    -- * Schema
    Schema,
    -- ** Schema proxies
    schema, tokens, user, pass,
    -- * Start
    db,
    userFact, passFact,
    db', db'',
    -- * Selecting
    val,
    token,
    ) where

import Futurice.Prelude
import Futurice.Monisto

-------------------------------------------------------------------------------
-- Schema definitions
-------------------------------------------------------------------------------

-- | In this tutorial we will work with simple schema.
type Schema =
    '[ '("tokens", TokenEntity)
     , '("endpoint", EndpointEntity)
     ]

type TokenEntity =
    '[ '("user", Text)
     , '("pass", Text)
     ]

type EndpointEntity =
    '[ '("endpoint", Text)
     ]

schema :: Proxy Schema
schema = Proxy

tokens :: EntityType Schema "tokens"
tokens = mkEntityType 

endpoints :: EntityType Schema "endpoint"
endpoints = mkEntityType

user :: Attribute Schema "tokens" '("user", Text)
user = mkTextAttribute

pass :: Attribute Schema "tokens" '("pass", Text)
pass = mkTextAttribute

endpoint :: Attribute Schema "endpoint" '("endpoint", Text)
endpoint = mkTextAttribute

-------------------------------------------------------------------------------
-- First insert
-------------------------------------------------------------------------------

-- | Initial, empty database.
--
-- @
-- >>> db
-- MkDatabase []
-- @
db :: Database Schema
db = emptyDatabase

-- | Some facts to add
--
-- @ 
-- >>> userFact
-- EntityId 0 :=> fromList [AText :=> "oleg"]
-- @
userFact :: Fact Schema
userFact = mkFact (mkEntityId tokens 0) user "oleg"

passFact :: Fact Schema
passFact = mkFact (mkEntityId tokens 0) pass "v3ryS3cret"

-- | After inserting two facts:
--
-- @
-- >>> db'
-- MkDatabase [EntityId 0 :=> fromList [AText :=> "v3ryS3cret",AText :=> "oleg"]]
-- @
db' :: Database Schema
db' = applyFact (applyFact db userFact) passFact

db'' :: Database Schema
db'' = applyFacts db
    [ mkFact (mkEntityId tokens 1) user "lego"
    , mkFact (mkEntityId tokens 1) pass "batman"
    , mkFact (mkEntityId endpoints 2) endpoint "/"
    ]

-------------------------------------------------------------------------------
-- Selecting
-------------------------------------------------------------------------------

-- | Select resulting into generic product:
--
-- @
-- >>> val
-- Just (MkValue "oleg" :* (MkValue "v3ryS3cret" :* Nil))
-- @
val :: Maybe (NP (Value Schema "tokens") TokenEntity)
val = selectById' (mkEntityId tokens 0) db'

-- | Let's define better value
data Token = Token !Text !Text deriving (Show)
deriveGeneric ''Token

-- | But with a little template haskell magic, we can use select into real data:
--
-- @
-- >>> token
-- Just (Token "oleg" "v3ryS3cret")
-- @
token :: Maybe Token
token = selectById (mkEntityId tokens 0) db'
