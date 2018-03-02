{-# LANGUAGE OverloadedStrings #-}
module LambdaCSS.Printer where

import Data.ByteString.Builder.Scientific
       (FPFormat (..), formatScientificBuilder)
import Data.Char                          (isAscii, isLetter, ord)
import Data.List.NonEmpty                 (NonEmpty (..))
import Data.Scientific                    (Scientific)
import Data.Semigroup                     (Semigroup (..))
import Data.String                        (IsString (..))

import qualified Data.ByteString.Builder as B

import LambdaCSS.Types

stylesheet :: Stylesheet -> B.Builder
stylesheet = tsToBuilder . stylesheet'

stylesheet' :: Stylesheet -> TS
stylesheet' (Stylesheet ss) = foldMap statement ss

statement :: Statement -> TS
statement (Charset s)         = atKeyword "charset " <> string s <> punct ';' <> punct '\n'
statement (NestedStatement s) = nestedStatement s

nestedStatement :: NestedStatement -> TS
nestedStatement (Ruleset r)      = ruleset r
nestedStatement (Page dl)        = atKeyword "page" <> declarationList dl
nestedStatement (Fontface dl)    = atKeyword "font-face" <> declarationList dl
nestedStatement (Viewport dl)    = atKeyword "viewport" <> declarationList dl
nestedStatement (Keyframes n bs) = atKeyword "keyframes"
    <> ident n
    <> braces (foldMap keyframeBlock bs)
  where
    keyframeBlock :: KeyframeBlock -> TS
    keyframeBlock (KeyframeBlock ss dl) =
        foldMapSep1 keyframeSelector comma ss <> declarationList dl

    keyframeSelector :: KeyframeSelector -> TS
    keyframeSelector KeyframeSelectorFrom           = ident "from"
    keyframeSelector KeyframeSelectorTo             = ident "to"
    keyframeSelector (KeyframeSelectorPercentage p) = TS (TokPercentage p :)
nestedStatement (Media q ns) = atKeyword "media"
    <> foldMap any_ q
    <> braces (foldMap nestedStatement ns)

ruleset :: Ruleset -> TS
ruleset (RS ss dl) = foldMapSep1 selector comma ss <> declarationList dl

declarationList :: [Declaration] -> TS
declarationList = braces . foldMapSep declaration (punct ';')

selector :: Selector -> TS
selector (End s)  = simpleSelector s
selector (a :$ b) = simpleSelector a <> punct ' ' <> selector b
selector (a :> b) = simpleSelector a <> punct '>' <> selector b
selector (a :+ b) = simpleSelector a <> punct '+' <> selector b
selector (a :~ b) = simpleSelector a <> punct '~' <> selector b

simpleSelector :: SimpleSelector -> TS
simpleSelector (SimpleSelector Nothing [])  = punct '*'
simpleSelector (SimpleSelector Nothing ms)  = foldMap selectorMod ms
simpleSelector (SimpleSelector (Just n) ms) = ident n <> foldMap selectorMod ms

selectorMod :: SelectorMod -> TS
selectorMod (ClassSelector s)     = punct '.' <> ident s
selectorMod (IdSelector s)        = punct '#' <> ident s
selectorMod (PseudoElement s)     = punct ':' <> punct ':' <> ident s
selectorMod (PseudoSelector s xs) =
    punct ':' <> ident s <> foldMap (parens . foldMap any_) xs
selectorMod (AttributeSelector s op) = brackets $
    ident s <> foldMap (\(o, x) -> attributeOp o <> identString x) op

attributeOp :: AttributeOp -> TS
attributeOp AttrOpEq       = punct '='
attributeOp AttrOpWord     = punct '~' <> punct '='
attributeOp AttrOpPrefix   = punct '^' <> punct '='
attributeOp AttrOpSuffix   = punct '$' <> punct '='
attributeOp AttrOpContains = punct '*' <> punct '='

declaration :: Declaration -> TS
declaration (Declaration n v) = ident n <> punct ':' <> value v

value :: Value -> TS
value = foldMapSep1 any_ mempty

any_ :: Any -> TS
any_ (Ident s)       = ident s
any_ (Number n)      = TS (TokNumber n :)
any_ (Dimension n u) = TS (TokDimension n u :)
any_ (Percentage n)  = TS (TokPercentage n :)
any_ (Delim c)       = punct c
any_ (URI s)         = ident "url" <> punct '(' <> string s <> punct ')'
any_  Colon          = punct ':'
any_ (Hash s)        = TS (TokHash s :)
any_ (String s)      = string s
any_ (Parens xs)     = parens (foldMap any_ xs)
any_ (Brackets xs)   = brackets (foldMap any_ xs)

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

data Token
    = TokIdent String
    | TokAtKeyword String
    | TokString String
    | TokHash String
    | TokNumber Scientific
    | TokPercentage Scientific
    | TokDimension Scientific String
    | TokPunct Char
  deriving (Eq, Show)

newtype TS = TS { runTS :: [Token] -> [Token] }

instance Semigroup TS where
    TS a <> TS b = TS (a . b)

instance Monoid TS where
    mempty = TS id
    mappend = (<>)

tsToBuilder :: TS -> B.Builder
tsToBuilder ts = iter (TokPunct ' ') (runTS ts []) where
    iter _ [] = mempty
    iter x (y:ys) = tok x y <> iter y ys

    sep TokDimension {} = fspace
    sep TokIdent {}     = fspace
    sep TokHash {}      = fspace
    sep TokAtKeyword {} = fspace
    sep TokNumber {}    = fspace
    sep (TokPunct '-')  = fspace
    sep (TokPunct '+')  = fspace
    sep _ = id
    
    fspace = (B.char8 ' ' <>)

    tok p (TokIdent s)       = sep p $ fromString s
    tok _ (TokAtKeyword s)   = B.char8 '@' <> fromString s
    tok _ (TokHash s)        = B.char8 '#' <> fromString s
    tok p (TokNumber n)      = sep p $ sci n
    tok p (TokPercentage n)  = sep p $ sci n <> B.char8 '%'
    tok p (TokDimension n s) = sep p $ sci n <> fromString s
    tok p (TokPunct c)       
          | c == '(' && p == TokIdent "and" = fspace (B.char8 c)
          | otherwise        = B.char8 c
    tok _ (TokString s)      = B.char8 '"' <> foldMap schar s <> B.char8 '"' where
        schar c
            | c == '\n' = "\\n"
            | c == '\r' = "\\r"
            | c == '\t' = "\\t"
            | c == '\f' = "\\f"
            | isAscii c = B.char8 c
            | otherwise = B.char8 '\\' <> B.wordHex (fromIntegral (ord c)) <> B.char8 ' '

    sci = formatScientificBuilder Fixed Nothing

-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------

identString :: String -> TS
identString s
    | all (\x -> isLetter x && isAscii x) s = ident s
    | otherwise                             = string s

ident :: String -> TS
ident s = TS (TokIdent s :)

string :: String -> TS
string s = TS (TokString s :)

atKeyword :: String -> TS
atKeyword s = TS (TokAtKeyword s :)

punct :: Char -> TS
punct c = TS (TokPunct c :)

comma :: TS
comma = punct ','

brackets :: TS -> TS
brackets x = punct '[' <> x <> punct ']'

braces :: TS -> TS
braces x = punct '{' <> x <> punct '}'

parens :: TS -> TS
parens x = punct '(' <> x <> punct ')'

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

foldMapSep :: (Semigroup s, Monoid s) => (a -> s) -> s -> [a] -> s
foldMapSep _f _sep []       = mempty
foldMapSep  f  sep (x : xs) = foldMapSep1 f sep (x :| xs)

foldMapSep1 :: (Semigroup s, Monoid s) => (a -> s) -> s -> NonEmpty a -> s
foldMapSep1 f sep (x :| xs) = f x <> foldMap (\y -> sep <> f y) xs
