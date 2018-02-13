module Futurice.App.EnumGenerator where

import Futurice.Prelude
import Prelude ()

import Algebra.Lattice              ((\/))
import Data.Char                    (isAlphaNum, isLower, isUpper, toLower)
import Text.Trifecta                ((<?>))
import Text.Trifecta.Parser         (parseByteString)

import qualified Data.ByteString              as BS
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as T

generateEnum :: BS.ByteString -> Either String BS.ByteString
generateEnum bs = case parseByteString (parser <* T.eof) mempty bs of
    T.Success x -> Right $ encodeUtf8 (show (ppModule x) ^. packed)
    T.Failure e -> Left $ show $ T._errDoc e

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

data Statement
    = ImportDef
    | EnumDef String [String] [String]
    | RawDef String
  deriving Show

parser :: T.TokenParsing m => m [Statement]
parser = many statement

statement :: T.TokenParsing m => m Statement
statement = importDef <|> rawDef <|> enumDef

importDef :: T.TokenParsing m => m Statement
importDef = ImportDef <$ T.symbol "\\imports"

enumDef :: T.TokenParsing m => m Statement
enumDef = pure EnumDef
    <*  T.symbol "\\enum"
    <*> T.braces conName
    <*> T.braces (T.sepBy T.stringLiteral T.comma)
    <*> T.braces (T.sepBy conName T.comma)

rawDef :: T.TokenParsing m => m Statement
rawDef = pure RawDef
    <*  rawBegin
    <*> T.manyTill T.anyChar (T.try rawEnd)
  where
    rawBegin = T.symbol "\\begin{code}"
    rawEnd   = T.symbol "\\end{code}"

conName :: T.TokenParsing m => m String
conName = T.token $ (:) <$> x <*> many y <?> "Conname" where
    x = T.satisfy isUpper
    y = T.satisfy $ isAlphaNum \/ (`elem` "_'")

varName :: T.TokenParsing m => m String
varName = T.token $ (:) <$> x <*> many y <?> "Conname" where
    x = T.satisfy $ isLower \/ (`elem` "_'")
    y = T.satisfy isAlphaNum

-------------------------------------------------------------------------------
-- Pretty-Printer
-------------------------------------------------------------------------------

ppModule :: [Statement] -> PP.Doc
ppModule xs = PP.vsep
    [ note
    , pragmas
    , PP.vsep (map ppStatement xs)
    ]
  where
    note = PP.text "-- This module is generated with futurice-enum-generator"
    pragmas = PP.vsep $ map PP.text
        [ "{-# LANGUAGE DataKinds          #-}"
        , "{-# LANGUAGE OverloadedStrings  #-}"
        , "{-# LANGUAGE RecordWildCards    #-}"
        , "{-# LANGUAGE StandaloneDeriving #-}"
        , "{-# LANGUAGE TemplateHaskell    #-}"
        , "{-# LANGUAGE TypeFamilies       #-}"
        ]

ppStatement :: Statement -> PP.Doc
ppStatement (RawDef code)        = PP.string code
ppStatement (EnumDef name ns cs) = ppEnumDef name ns cs
ppStatement ImportDef            = ppImports

ppImports :: PP.Doc
ppImports = PP.string $ unlines
    [ "import Futurice.Generics"
    , "import Futurice.Generics.Enum"
    , "import Futurice.Prelude"
    , "import Lucid                  (ToHtml (..))"
    , "import Prelude ()"
    , ""
    , "import qualified Data.Csv as Csv"
    ]

ppEnumDef :: String -> [String] -> [String] -> PP.Doc
ppEnumDef name ns cs = PP.vsep
    [ templateHaskell
    , ei
    , toText
    , fromText
    , standalone
    , textprism
    , nfdata
    , binary
    , arbitrary
    , tohtml
    , toparamschema
    , toschema
    , tojson
    , fromjson
    , fromhttpapidata
    , tohttpapidata
    , csvtofield
    , csvfromfield
    ]
  where
    templateHaskell = PP.string $ unlines
        [ ""
        , "makePrisms    ''" ++ name
        , "deriveGeneric ''" ++ name
        , "deriveLift    ''" ++ name
        ]

    ei = PP.text ("ei :: EnumInstances " ++ name)
        PP.<$> PP.text "ei = sopEnumInstances $"
        PP.<$> PP.indent 4 (PP.vsep (map ei' ns) PP.<$> PP.text "Nil")
        PP.<$> mempty
      where
        ei' c = PP.text $ "K " ++ show c ++ " :*"

    toText          = template
        [ "#ToText :: $ -> Text"
        , "#ToText = enumToText ei"
        ]
    fromText        = template
        [ "#FromText :: Text -> Maybe $"
        , "#FromText = enumFromText ei"
        ]
    textprism       = template
        [ "_$ :: Prism' Text $"
        , "_$ = enumPrism ei"
        ]
    standalone      = template
        [ "deriving instance Eq $"
        , "deriving instance Ord $"
        , "deriving instance Show $"
        , "deriving instance Read $"
        , "deriving instance Enum $"
        , "deriving instance Bounded $"
        , "deriving instance Generic $"
        , "deriving instance Typeable $"
        ]
    nfdata          = inst "NFData" ["instance NFData $"]
    binary          = inst "Binary" ["instance Binary $"]
    arbitrary       = inst "Arbitrary"
        [ "instance Arbitrary $ where"
        , "    arbitrary = sopArbitrary"
        , "    shrink    = sopShrink"
        ]
    tohtml          = inst "ToHtml"
        [ "instance ToHtml $ where"
        , "    toHtmlRaw = toHtml"
        , "    toHtml = toHtml . enumToText ei"
        ]
    toparamschema   = inst "ToParamSchema"
        [ "instance ToParamSchema $ where"
        , "    toParamSchema = enumToParamSchema ei"
        ]
    toschema        = inst "ToSchema"
        [ "instance ToSchema $ where"
        , "    declareNamedSchema = enumDeclareNamedSchema ei"
        ]
    tojson          = inst "ToJSON"
        [ "instance ToJSON $ where"
        , "    toJSON     = enumToJSON ei"
        , "    toEncoding = enumToEncoding ei"
        ]
    fromjson        = inst "FromJSON"
        [ "instance FromJSON $ where"
        , "    parseJSON = enumParseJSON ei"
        ]
    fromhttpapidata = inst "FromHttpApiData"
        [ "instance FromHttpApiData $ where"
        , "    parseUrlPiece = enumParseUrlPiece ei"
        ]
    tohttpapidata   = inst "ToHttpApiData"
        [ "instance ToHttpApiData $ where"
        , "    toUrlPiece = enumToUrlPiece ei"
        ]
    csvtofield      = inst "Csv.ToField"
        [ "instance Csv.ToField $ where"
        , "    toField = enumCsvToField ei"
        ]
    csvfromfield    = inst "Csv.FromField"
        [ "instance Csv.FromField $ where"
        , "     parseField = enumCsvParseField ei"
        ]

    -- $ expands into type name
    -- # expands into lower-cased type name
    template = PP.string . concatMap rep . unlines
      where
        rep '$' = name
        rep '#' = name & ix 0 %~ toLower
        rep c   = [c]

    inst cls ts
        | cls `elem` cs = mempty
        | otherwise     = template ts
