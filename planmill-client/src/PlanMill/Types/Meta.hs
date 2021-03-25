{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Meta (Meta(..), lookupFieldEnum) where

import PlanMill.Internal.Prelude

import Data.Aeson.Types (Parser)
import Data.Char        (isSpace)

import qualified Data.HashMap.Strict         as HM
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Text.Regex.Applicative.Text as RE

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..))

data FieldFormat
    = FieldFormatText
    | FieldFormatEnum !Text
    | FieldFormatOther !Text
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance NFData FieldFormat
instance AnsiPretty FieldFormat
instance Binary FieldFormat

data MetaField = MetaField
    { _metaFieldFormat  :: !FieldFormat
    , _metaFieldCaption :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance NFData MetaField
instance AnsiPretty MetaField where ansiPretty = ansiPretty . show
instance Binary MetaField

data Meta = Meta
    { metaFields  :: HashMap Text MetaField
    , metaFilters :: HashMap Text (HashMap Text Text)
    }
    deriving (Eq, Show, Read, Generic, Typeable)

instance NFData Meta
instance AnsiPretty Meta
instance Binary Meta

deriveGeneric ''FieldFormat
deriveGeneric ''MetaField
deriveGeneric ''Meta

instance Structured FieldFormat
instance Structured MetaField
instance Structured Meta

instance FromJSON FieldFormat where
    parseJSON = withText "Field format" $ \t ->
        pure . fromMaybe (FieldFormatOther t) $ RE.match re t
      where
        re     = FieldFormatText <$ RE.string "text" <|> enumRe
        enumRe = FieldFormatEnum . T.pack
            <$ "Enumeration"
            <* RE.few (RE.psym isSpace)
            <* "values."
            <*> many RE.anySym

instance FromJSON MetaField where
    parseJSON = withObject "Field meta" $ \obj -> MetaField
        <$> obj .:? "format" .!= FieldFormatText
        <*> obj .:? "caption" .!= ""

toHashMap :: Vector Value -> Parser (HashMap Text (HashMap Text Text))
toHashMap vec =
    let toFilterOption obj = do
            name <- obj .: "name"
            values <- obj .:? "values"
            pure (name, fromMaybe mempty values)
    in HM.fromList . V.toList <$> mapM (withObject "filter" toFilterOption) vec

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \obj -> Meta
        <$> obj .:? "fields" .!= mempty
        <*> (obj .:? "filters" >>= traverse toHashMap) .!= mempty

lookupFieldEnum
    :: Meta
    -> Text  -- ^ field name
    -> Maybe Text
lookupFieldEnum m f =
    case HM.lookup f (metaFields m) of
        Nothing                                 -> Nothing
        Just (MetaField (FieldFormatEnum e) _ ) -> Just e
        _                                       -> Nothing
