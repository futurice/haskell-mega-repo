{-# LANGUAGE BangPatterns #-}
-- | The parser is based on
-- /Cascading Style Sheets Level 2 Revision 2 (CSS 2.2) Specification/
-- (W3C First Public Working Draft 12 April 2016)
-- <https://www.w3.org/TR/CSS22/syndata.html#tokenization>
module LambdaCSS.Parser where

import Control.Applicative (Alternative (..), liftA2, optional, (<**>))
import Data.Char           (chr)
import Data.Functor        (void)
import Data.List.NonEmpty  (NonEmpty (..))
import Text.Trifecta
       (CharParsing (..), Parsing (..), choice, oneOf, sepBy)

import qualified Data.Scientific as S
import qualified Text.Trifecta   as T

import LambdaCSS.Types

-- * Core syntax

-- |
-- @
-- stylesheet  : [ CDO | CDC | S | statement ]*;
-- @
--
-- /TODO:/ Currently parser doesn't recognise /CDO/ and /CDC/ tokens.
stylesheet :: CharParsing m => m Stylesheet
stylesheet = Stylesheet <$> many (ws *> statement) <* ws

-- |
-- @
-- statement   : ruleset | at-rule;
-- @
--
-- For recognised at-rules see 'atRule'.
statement :: CharParsing m => m Statement
statement = choice
    [ charset
    , NestedStatement <$> nestedStatement
    ]
  where
    charset = string "@charset"
        *> skipSome (oneOf " \t\r\n")
        *> (Charset <$> string_)
        <* semicolon
        <* ws

-- |
--
-- @
-- at-rule     : ATKEYWORD S* any* [ block | ';' S* ];
-- block       : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
--
-- ATKEYWORD    @{ident}
-- @
--
-- @
-- nested_statement
--   : ruleset | media | page | font_face_rule | keyframes_rule |
--     supports_rule
--   ;
-- @
--
-- === Charset
--
-- Charset rule is recognised, but at the moment not used.
--
-- @
-- /\@charset/ = @charset "/charset/";
-- @
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS/%40charset>
--
-- === Media
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS/@media>
--
-- === Page
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS/@page>
--
-- === Keyframes
--
-- Keyframes are specified using the @keyframes at-rule, defined as follows:
--
-- @
-- /\@keyframes/        = @keyframes /keyframes-name/ { /rule-list/ }
-- /keyframes-name/    = /custom-ident/ | /string/
-- /keyframe-block/    = /keyframe-selector/# { /declaration-list/ }
-- /keyframe-selector/ = from | to | /percentage/
-- @
--
-- The /rule-list/ inside of @\@keyframes@ can only contain /keyframe-block/ rules.
--
-- <https://www.w3.org/TR/css-animations-1/#keyframes>
-- <https://developer.mozilla.org/en-US/docs/Web/CSS/%40keyframes>
--
nestedStatement :: CharParsing m => m NestedStatement
nestedStatement = Ruleset <$> ruleset <|> char '@' *> choice
    [ media
    , page
    , keyframes
    ] <?> "nested statement"
  where
    -- media queries
    media = string "media"
        *> ws
        *> (Media <$> mediaquery <*> groupRuleBody)

    mediaquery :: CharParsing m => m [Any]
    mediaquery = many $ choice
        [ try $ Ident <$> ident -- we need try as number can start with minus/dash - too
        , try numeric -- dot number
        , Hash <$> hash
        , String <$> string_
        , Colon <$ colon
        , Parens <$ lparen <* ws <*> many any_ <* rparen
        , Delim <$> delim
        ] <* ws

    -- page
    page = string "page" *> ws *> page'
    page' = Page <$> braces declarationList

    -- Keyframes
    keyframes = string "keyframes" *> ws *> keyframes'
    keyframes' = Keyframes
        <$> (ident <|> string_) -- todo: custom-ident
        <*  ws
        <*> braces (many keyframeBlock)

    keyframeBlock = KeyframeBlock
        <$> manyHash keyframeSelector
        <*> braces declarationList

    keyframeSelector = choice
        [ KeyframeSelectorFrom <$ string "from"
        , KeyframeSelectorTo <$ string "to"
        , KeyframeSelectorPercentage <$> num <* char '%'
        ]

-- |
-- @
-- group_rule_body
--   : '{' S* nested_statement* '}' S*
--   ;
-- @
groupRuleBody :: CharParsing m => m [NestedStatement]
groupRuleBody = braces (many nestedStatement)

-- |
-- @
-- ruleset     : selector? '{' S* declaration-list '}' S*;
-- @
--
-- /Note:/ selector is marked optional, but we always require one.
ruleset :: CharParsing m => m Ruleset
ruleset = RS
    <$> manyHash selector
    <*> braces declarationList

-- |
-- @
-- declaration-list: declaration [ ';' S* declaration-list ]?
--               | at-rule declaration-list
--               | /* empty */;
-- @
--
-- We don't recognise at-rules inside declaration lists.
declarationList :: CharParsing m => m DeclarationList
declarationList = sepBy declaration (semicolon *> ws)

-- |
-- @
-- selector    : any+;
-- @
--


selector :: CharParsing m => m Selector
selector = simpleSelector <**> choice
    [ ws1                  *> (flip (:$) <$> selector)
    , ws *> char '>' *> ws *> (flip (:>) <$> selector)
    , ws *> char '+' *> ws *> (flip (:+) <$> selector)
    , ws *> char '~' *> ws *> (flip (:~) <$> selector)
    , pure End
    ]

simpleSelector :: CharParsing m => m SimpleSelector
simpleSelector = SimpleSelector
    <$> (Nothing <$ char '*' <|> optional ident)
    <*> many selectorMod
  where
    selectorMod = choice
        [ ClassSelector <$ char '.' <*> ident
        , IdSelector <$ char '#' <*> ident
        , char ':' *> pseudo
        , attributeSelector
        ]

    attributeSelector = AttributeSelector
        <$  char '['
        <*> ident
        <*> optional (liftA2 (,) attributeOp (ident <|> string_))
        <*  char ']'

    attributeOp = choice
        [ AttrOpEq       <$ string "="
        , AttrOpWord     <$ string "~="
        , AttrOpPrefix   <$ string "^="
        , AttrOpSuffix   <$ string "$="
        , AttrOpContains <$ string "*="
        ]

    pseudo = choice
        [ PseudoSelector <$> ident <*> optional (parens (many any_))
        , PseudoElement <$ char ':' <*> ident
        ]

-- A selector is a chain of one or more simple selectors separated by
-- combinators. Combinators are: white space, ">", and "+". White space may
-- appear between a combinator and the simple selectors around it.

-- |
-- @
-- declaration : property S* ':' S* value;
-- property    : IDENT;
-- value       : [ any | block | ATKEYWORD S* ]+;
-- @
--
-- TODO: value: block ATKEYWORD
declaration :: CharParsing m => m Declaration
declaration = Declaration
    <$> ident
    <*  (ws *> colon *> ws)
    <*> value
  where
    value = some1 any_

-- |
-- @
-- any         : [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
--               | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
--               | DASHMATCH | ':' | FUNCTION S* [any|unused]* ')'
--               | '(' S* [any|unused]* ')' | '[' S* [any|unused]* ']'
--               ] S*;
-- @
any_ :: CharParsing m => m Any
any_ = choice
    [ try $ Ident <$> ident -- we need try as number can start with minus/dash - too
    , try numeric -- dot number
    , Hash <$> hash
    , String <$> string_
    , Colon <$ colon
    , Parens <$ lparen <* ws <*> many any_ <* rparen
    , Brackets <$ lbracket <* ws <*> many any_ <* rbracket
    , Delim <$> delim
    ] <* ws

-- |
-- @
-- NUMBER     {num}
-- PERCENTAGE {num}%
-- DIMENSION  {num}{ident}
-- @
numeric :: CharParsing m => m Any
numeric = num <**> u where
  u = choice
      [ Percentage <$ char '%'
      , flip Dimension <$> ident
      , pure Number
      ]

-- * Tokens

-- | @S*@
ws :: CharParsing m => m ()
ws = skipMany (void (oneOf " \t\r\n\f") <|> comment)

-- | @S@
ws1 :: CharParsing m => m ()
ws1 = skipSome $ oneOf " \t\r\n\f"

-- |
-- @
-- IDENT        {ident}
-- ident        [-]?{nmstart}{nmchar}*
-- nmstart      [_a-z]|{nonascii}|{escape}
-- nonascii     [^\0-\177]
-- unicode      \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
-- escape       {unicode}|\\[^\n\r\f0-9a-f]
-- @
ident :: CharParsing m => m String
ident = mk <$> optional (char '-') <*> nmstart <*> many nmchar <?> "ident"
  where
    mk x y zs = maybe id (:) x (y : zs)
    -- TODO:  nonascii | escape
    nmstart = satisfy $ \c -> c == '_'
        || (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')

-- |
-- @
-- nmchar       [_a-z0-9-]|{nonascii}|{escape}
-- @
--
-- TODO: nonascii | escape
nmchar :: CharParsing m => m Char
nmchar = satisfy $ \c -> c == '_' || c == '-'
    || (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')

-- |
-- @
-- num  [+-]?([0-9]+|[0-9]*\.[0-9]+)(e[+-]?[0-9]+)?
-- @
num :: CharParsing m => m S.Scientific
num = mk <$> optional sign <*> (coeff1 <|> coeff2) <*> optional expon <?> "num"
  where
    mk s c _e = maybe id id s (fromRational c)

    sign = negate <$ char '-' <|> id <$ char '+' <?> "sign"
    coeff1 = c <$> some T.digit <*> optional (char '.' *> some T.digit) where
        c a b = fromInteger (read a)
              + maybe 0 (\b' -> fromInteger (read b') * 10 ^^ negate (length b')) b
    coeff2 = char '.' *> (c <$> some T.digit) where
        c b = fromInteger (read b) * 10 ^^ negate (length b)
    expon = unexpected "TODO"

-- |
-- @
-- string       {string1}|{string2}
-- string1      \"([^\n\r\f\\"]|\\{nl}|{escape})*\"
-- string2      \'([^\n\r\f\\']|\\{nl}|{escape})*\'
-- unicode	    \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
-- escape       {unicode}|\\[^\n\r\f0-9a-f]
-- @
string_ :: CharParsing m => m String
string_ = str '"' <|> str '\'' where
    str c = char c *> many (ch <|> unicode <|> escape) <* char c where
        ch = satisfy $ \x -> x `notElem` (c : "\n\r\f\\")

    unicode = char '\\' *> unicode' <* optional (void (try (string "\r\n")) <|> void (oneOf " \n\r\t\f"))
    unicode' = chr . readHex <$> range 1 6 T.hexDigit
    escape = empty -- todo: non special characters

-- |
-- @
-- HASH         #{name}
-- name         {nmchar}+
-- @
hash :: CharParsing m => m String
hash = char '#' *> some nmchar <?> "hash"

-- | @:@
colon :: CharParsing m => m ()
colon = void (char ':') <?> "colon"

-- | @;@
semicolon :: CharParsing m => m ()
semicolon = void (char ';')

-- | @{@
lbrace :: CharParsing m => m ()
lbrace = void (char '{')

-- | @}@
rbrace :: CharParsing m => m ()
rbrace = void (char '}')

-- | @(@
lparen :: CharParsing m => m ()
lparen = void (char '(')

-- | @)@
rparen :: CharParsing m => m ()
rparen = void (char ')')

-- | @[@
lbracket :: CharParsing m => m ()
lbracket = void (char '[')

-- | @]@
rbracket :: CharParsing m => m ()
rbracket = void (char ']')

-- | @DELIM@    any other character not matched by the above rules, and neither a single nor a double quote
--
-- tmp
delim :: CharParsing m => m Char
delim = oneOf ".-=!,>*+^~\\" <?> "delim"

-- ** CSS utitilities

-- | Comments begin with the characters "/*" and end with the characters "*/".
-- They may occur anywhere outside other tokens, and their contents have no
-- influence on the rendering. Comments may not be nested.
comment :: CharParsing m => m ()
comment = string "/*" *> void (T.manyTill anyChar (try (string "*/")))

-- |
--
-- The hash mark multiplier indicates that the entity may be repeated one or
-- more times (for example, the plus multiplier), but each occurrence is
-- separated by a comma (',').
--
manyHash :: CharParsing m => m a -> m (NonEmpty a)
manyHash p = (:|) <$> p <*> many (sep *> p) where
    sep = ws *> char ',' *> ws

-- | '{' S* p '}' S*
braces :: CharParsing m => m a -> m a
braces p = lbrace *> ws *> p <* rbrace <* ws

-- | '(' S* p ')'
parens :: CharParsing m => m a -> m a
parens p = lparen *> ws *> p <* rparen


-- ** Parser utilities

some1 :: Alternative f => f a -> f (NonEmpty a)
some1 p = (:|) <$> p <*> many p

range :: Alternative m => Int -> Int -> m a -> m [a]
range m' n' p = go m' n'
  where
    go !m !n
      | n <= 0 || m > n = pure []
      | m > 0           = liftA2 (:) p (go (m - 1) (n - 1))
      | otherwise       = liftA2 (:) p (go 0 (n - 1)) <|> pure []

readHex :: Num a => String -> a
readHex = go 0 where
    go acc [] = acc
    go acc ('0':cs) = go (acc * 16) cs
    go acc ('1':cs) = go (acc * 16 + 1) cs
    go acc ('2':cs) = go (acc * 16 + 2) cs
    go acc ('3':cs) = go (acc * 16 + 3) cs
    go acc ('4':cs) = go (acc * 16 + 4) cs
    go acc ('5':cs) = go (acc * 16 + 5) cs
    go acc ('6':cs) = go (acc * 16 + 6) cs
    go acc ('7':cs) = go (acc * 16 + 7) cs
    go acc ('8':cs) = go (acc * 16 + 8) cs
    go acc ('9':cs) = go (acc * 16 + 9) cs
    go acc ('a':cs) = go (acc * 16 + 10) cs
    go acc ('b':cs) = go (acc * 16 + 11) cs
    go acc ('c':cs) = go (acc * 16 + 12) cs
    go acc ('d':cs) = go (acc * 16 + 13) cs
    go acc ('e':cs) = go (acc * 16 + 14) cs
    go acc ('f':cs) = go (acc * 16 + 15) cs
    go acc ('A':cs) = go (acc * 16 + 10) cs
    go acc ('B':cs) = go (acc * 16 + 11) cs
    go acc ('C':cs) = go (acc * 16 + 12) cs
    go acc ('D':cs) = go (acc * 16 + 13) cs
    go acc ('E':cs) = go (acc * 16 + 14) cs
    go acc ('F':cs) = go (acc * 16 + 15) cs
    go acc (_:cs)   = go (acc * 16) cs
