module LambdaCSS (
    parseLambdaCSS,
    printLambdaCSS,
    module LambdaCSS.Types,
    -- * Traversals
    HasHashes(..),
    ) where

import LambdaCSS.Types

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified LambdaCSS.Parser        as Parser
import qualified LambdaCSS.Printer       as Printer
import qualified Text.Trifecta           as T

parseLambdaCSS :: BS.ByteString -> Either String Stylesheet
parseLambdaCSS bs = case T.parseByteString (Parser.stylesheet <* T.eof) mempty bs of
    T.Success x -> Right x
    T.Failure e -> Left (show (T._errDoc e))

printLambdaCSS :: Stylesheet -> LBS.ByteString
printLambdaCSS = B.toLazyByteString . Printer.stylesheet

-- | Hashes in declaration lists.
class HasHashes a where
    hashes :: Applicative f => (String -> f String) -> a -> f a

instance HasHashes Stylesheet where
    hashes f (Stylesheet x) = Stylesheet <$> traverse (hashes f) x

instance HasHashes Statement where
    hashes f (NestedStatement x) = NestedStatement <$> hashes f x
    hashes _ x                   = pure x

instance HasHashes NestedStatement where
    hashes f (Ruleset x)     = Ruleset <$> hashes f x
    hashes f (Media q x)     = Media q <$> traverse (hashes f) x
    hashes f (Page dl)       = Page <$> traverse (hashes f) dl
    hashes f (Keyframes n x) = Keyframes n <$> traverse (hashes f) x
    hashes f (Fontface dl)   = Fontface <$> traverse (hashes f) dl
    hashes f (Viewport dl)   = Viewport <$> traverse (hashes f) dl

instance HasHashes KeyframeBlock where
    hashes f (KeyframeBlock ss dl) = KeyframeBlock ss <$> traverse (hashes f) dl

instance HasHashes Ruleset where
    hashes f (RS ss dl) = RS ss <$> traverse (hashes f) dl

instance HasHashes Declaration where
    hashes f (Declaration n v) = Declaration n <$> traverse (hashes f) v

instance HasHashes Any where
    hashes f (Hash s) = Hash <$> f s
    hashes _ x        = pure x
