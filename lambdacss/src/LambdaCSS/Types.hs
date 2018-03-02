{-# LANGUAGE DeriveGeneric #-}
module LambdaCSS.Types (
    -- * Types
    Stylesheet (..),
    Statement (..),
    NestedStatement (..),
    -- ** Style rules
    Ruleset (..),
    DeclarationList,
    Declaration (..),
    -- ** Selectors
    Selector (..),
    SimpleSelector (..),
    SelectorMod (..),
    AttributeOp (..),
    -- ** Keyframes
    KeyframeBlock (..),
    KeyframeSelector (..),
    -- ** Media
    MediaQuery,
    -- ** Value
    Value,
    Any (..),
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific    (Scientific)
import GHC.Generics       (Generic)

-- | Stylesheet is a sequence of statements.
newtype Stylesheet = Stylesheet [Statement]
  deriving (Eq, Show, Generic)

-- | Top level statements
data Statement
    = NestedStatement NestedStatement
    | Charset String  -- ^ @\@charset@
  deriving (Eq, Show, Generic)

-- | Conditional groups can be nested.
data NestedStatement
    = Ruleset Ruleset                     -- ^ Style rules
    | Media MediaQuery [NestedStatement]  -- ^ @\@media@
    | Page DeclarationList                -- ^ @\@page@
    | Fontface DeclarationList            -- ^ @\@font-face@
    | Viewport DeclarationList            -- ^ @\@viewport@
    | Keyframes String [KeyframeBlock]    -- ^ @\@keyframes@
  deriving (Eq, Show, Generic)

-- ** Keyframes

data KeyframeBlock = KeyframeBlock (NonEmpty KeyframeSelector) DeclarationList
  deriving (Eq, Show, Generic)

data KeyframeSelector
    = KeyframeSelectorFrom
    | KeyframeSelectorTo
    | KeyframeSelectorPercentage Scientific
  deriving (Eq, Show, Generic)

-- ** Media

-- | TODO
type MediaQuery = [Any]

-- ** Declaration lists

-- | CSS rule sets.
data Ruleset = RS (NonEmpty Selector) DeclarationList
  deriving (Eq, Show, Generic)

type DeclarationList = [Declaration]

data Declaration = Declaration String Value
  deriving (Eq, Show, Generic)

-- ** Selector

-- | A selector is a chain of one or more simple selectors separated by
-- combinators. Combinators are: white space, ">", and "+".
data Selector
    = End SimpleSelector
    | SimpleSelector :$ Selector
    | SimpleSelector :> Selector
    | SimpleSelector :+ Selector
    | SimpleSelector :~ Selector
  deriving (Eq, Show, Generic)

infixr 1 :$
infixr 1 :>
infixr 1 :+
infixr 1 :~

-- | A simple selector is either a type selector or universal selector followed
-- immediately by zero or more attribute selectors, ID selectors, or
-- pseudo-classes, in any order. The simple selector matches if all of its
-- components match.
data SimpleSelector = SimpleSelector
    (Maybe String)
    [SelectorMod]
  deriving (Eq, Show, Generic)

-- |
--
-- TODO: case sensitivity is level 4: https://drafts.csswg.org/selectors-4/
data SelectorMod
    = AttributeSelector String (Maybe (AttributeOp, String))
    | IdSelector String
    | ClassSelector String
    | PseudoSelector String (Maybe [Any])
    | PseudoElement String
  deriving (Eq, Show, Generic)

data AttributeOp
    = AttrOpEq       -- @=@
    | AttrOpWord     -- @~=@
    | AttrOpPrefix   -- @^=@
    | AttrOpSuffix   -- @$=@
    | AttrOpContains -- @*=@
  deriving (Eq, Show, Generic)

-- ** Value

type Value = NonEmpty Any

data Any
    = Ident String
    | Number Scientific
    | Percentage Scientific
    | Dimension Scientific String
    | String String
    | Delim Char
    | URI String
    | Hash String
    --  UnicodeRange
    --  Includes
    --  DashMatch
    | Colon
    -- -| Function [Any]
    | Parens [Any]
    | Brackets [Any]
  deriving (Eq, Show, Generic)
