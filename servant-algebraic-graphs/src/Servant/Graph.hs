{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Servant.Graph (ALGA, Graph (..)) where

import Control.DeepSeq                (NFData (..))
import Data.Proxy                     (Proxy (..))
import Data.String                    (fromString)
import Data.Swagger                   (NamedSchema (..), ToSchema (..))
import Data.Text                      (Text)
import Data.Typeable                  (Typeable)
import GHC.TypeLits                   (KnownSymbol, Symbol, symbolVal)
import Servant.API                    (Accept (..), MimeRender (..))
import System.Process.ByteString.Lazy (readProcessWithExitCode)

import qualified Algebra.Graph           as G
import qualified Algebra.Graph.Class     as ALGA
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Network.HTTP.Media      as M

import qualified Algebra.Graph.Export.Dot as Dot

import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Servant
-------------------------------------------------------------------------------

data ALGA deriving Typeable

-- | @image/svg+xml@
instance Accept ALGA where
    contentType _ = "image" M.// "png"

instance (ALGA.ToGraph t, ALGA.ToVertex t ~ Text) => MimeRender ALGA t where
    mimeRender _ = renderDot

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

renderDot :: (ALGA.ToGraph g, ALGA.ToVertex g ~ Text) => g -> LBS.ByteString
renderDot g
    = (\(_, x, _) -> x)
    $ unsafePerformIO
    $ readProcessWithExitCode "sfdp" ["-Tpng"] -- make configurable!
    $ LTE.encodeUtf8
    $ Dot.export style g
  where
    style = (Dot.defaultStyle LT.fromStrict)
        { Dot.graphAttributes =
            [ "overlap" Dot.:= "false"
            ]
        }

-------------------------------------------------------------------------------
-- Graph
-------------------------------------------------------------------------------

newtype Graph (name :: Symbol) = Graph (G.Graph Text)

-- https://github.com/snowleopard/alga/issues/25
instance NFData (Graph name) where
    rnf (Graph _) = rnf ()

instance ALGA.Graph (Graph name) where
    type Vertex (Graph name) = Text

    empty = Graph G.empty
    vertex = Graph . G.vertex
    Graph a `overlay` Graph b = Graph (a `G.overlay` b)
    Graph a `connect` Graph b = Graph (a `G.connect` b)

instance ALGA.ToGraph (Graph name) where
    type ToVertex (Graph name) = Text
    toGraph (Graph g) = ALGA.toGraph g

instance KnownSymbol name => ToSchema (Graph name) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ fromString $ "Graph" ++ n) mempty
      where
        n = symbolVal (Proxy :: Proxy name)
