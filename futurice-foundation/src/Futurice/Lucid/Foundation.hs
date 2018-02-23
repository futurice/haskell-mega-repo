{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Lucid.Foundation (
    L.commuteHtmlT,
    -- * Vendor
    vendorServer,
    -- * Grid
    row_,
    large_,
    largemed_,
    fullRow_,
    -- * Table
    table_,
    condensedTable_,
    sortableTable_,
    vertRow_,
    -- * Form
    optionSelected_,
    checkbox_,
    -- * Page
    HtmlPage (..),
    page_,
    PageParams,
    pageCss,
    pageJs,
    pageJQuery,
    defPageParams,
    -- * JavaScript
    JS,
    getJS,
    makeJS,
    embedJS,
    -- * Lucid
    module Lucid,
    attrfor_,
    forWith_,
    ) where

import Clay                           (Css, render)
import Data.FileEmbed                 (embedFile)
import Data.Swagger                   (NamedSchema (..), ToSchema (..))
import Futurice.JavaScript
import Futurice.JavaScript.TH
import Futurice.Lucid.Style           (css)
import Futurice.Prelude
import GHC.TypeLits                   (KnownSymbol, Symbol, symbolVal)
import Lucid                          hiding (for_, table_)
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Prelude ()
import Servant                        (Raw, Server)
import Servant.Swagger.UI.Internal    (mkRecursiveEmbedded)

import qualified Lucid      as L
import qualified Lucid.Base as L

-------------------------------------------------------------------------------
-- Lucid
-------------------------------------------------------------------------------

attrfor_ :: Text -> Attribute
attrfor_ = L.for_

-- | 'intersperse'd 'for_'.
forWith_ :: (Foldable t, Applicative f) => f () -> t a ->  (a -> f b) -> f ()
forWith_ sep xs f = foldr g (pure ()) xs
  where
    g a b = f a *> sep *> b

-------------------------------------------------------------------------------
-- Grid
-------------------------------------------------------------------------------

row_ :: Term arg result => arg -> result
row_ = termWith "div" [ class_ "row " ]

large_ :: Term arg result => Int -> arg -> result
large_ n = termWith "div_"
    [ class_ $ fromString $ "columns large-" ++ show n ++ " " ]

largemed_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
largemed_ n = div_
    [ class_ $ "columns large-" <> textShow n <> " medium-" <> textShow n ]

fullRow_ :: Monad m => HtmlT m () -> HtmlT m ()
fullRow_ = row_ . large_ 12

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

table_ :: Term arg result => arg ->  result
table_ = termWith "table" [ class_ "hover " ]

sortableTable_ :: Term arg result => arg ->  result
sortableTable_ = termWith "table" [ data_ "futu-sortable-table" "true", class_ "hover " ]

condensedTable_ :: Term arg result => arg ->  result
condensedTable_ = termWith "table" [ class_ "hover condensed " ]

-- | Row in a vertical table.
--
-- @
-- +------+--------+
-- | key1 | value1 |
-- +------+--------|
-- | key2 | value2 |
-- +------+--------+
-- @
--
vertRow_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
vertRow_ title h = tr_ $ do
    th_ [ style_ "white-space: nowrap" ] (toHtml title)
    td_ [ style_ "width: 100%" ] h

-------------------------------------------------------------------------------
-- Form
-------------------------------------------------------------------------------

optionSelected_ :: Term arg result => Bool -> arg -> result
optionSelected_ True  = termWith "option" [ selected_ "selected "]
optionSelected_ False = term "option"

checkbox_ :: Monad m => Bool -> [Attribute] -> HtmlT m ()
checkbox_ True  attrs = input_ $ [ type_ "checkbox", checked_ ] <> attrs
checkbox_ False attrs = input_ $ [ type_ "checkbox" ] <> attrs

-------------------------------------------------------------------------------
-- Page
-------------------------------------------------------------------------------

-- TODO: create submodule, move there

newtype HtmlPage (k :: Symbol) = HtmlPage (Html ())

instance KnownSymbol s => ToSchema (HtmlPage s) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ "Html page: " <> name) mempty
      where
        name = symbolVal (Proxy :: Proxy s) ^. packed

instance ToHtml (HtmlPage a) where
    toHtmlRaw = toHtml
    toHtml (HtmlPage h) = hoist (return . runIdentity) h

-------------------------------------------------------------------------------
-- PageParams
-------------------------------------------------------------------------------

data PageParams = PageParams
    { _pageCss    :: [Css]
    , _pageJs     :: [JS]
    , _pageJQuery :: Bool
    }

defPageParams :: PageParams
defPageParams = PageParams [] [] False

makeLenses ''PageParams

-- | Similar to 'Term' from @lucid@.
class Page arg result | result -> arg where
    -- | Page template.
    page_ :: Text -> arg -> result

instance Page (Html ()) (HtmlPage k) where
    page_ t = pageImpl t defPageParams

instance (a ~ Html (), b ~ HtmlPage k) => Page PageParams (a -> b) where
    page_ = pageImpl

pageImpl :: Text -> PageParams -> Html () -> HtmlPage k
pageImpl t p b = HtmlPage $ doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml t
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        -- Stylesheets
        link_ [ rel_ "stylesheet", href_ "/vendor/foundation.min.css" ]
        link_ [ rel_ "stylesheet", href_ "/vendor/jquery-ui.min.css" ]
        link_ [ rel_ "stylesheet", href_ "/vendor/select2.min.css" ]
        -- JS
        script_ [ src_ "/vendor/jquery-3.1.1.min.js" ] tempty
        script_ [ src_ "/vendor/jquery-ui.min.js" ] tempty
        script_ [ src_ "/vendor/select2.min.js" ] tempty
        script_ [ src_ "/vendor/foundation.min.js"] tempty
        script_ [ src_ "/vendor/lodash.js" ] tempty
        script_ [ src_ "/vendor/menrva.standalone.js" ] tempty
        script_ [ src_ "/vendor/futu.js" ] tempty -- vendor, even done by us :)

        -- Futurice styles
        style_ $ view strict $ render css
        -- additional styles
        for_ (p ^. pageCss) $ style_ . view strict . render
        -- additional js
        for_ (p ^. pageJs) $ toHtml
    body_ b
  where
    tempty = "" :: Text

-------------------------------------------------------------------------------
-- Statics
-------------------------------------------------------------------------------

vendorFiles :: [(FilePath, ByteString)]
vendorFiles
    = ("/futu.js", $(embedFile "futu.js"))
    : $(mkRecursiveEmbedded "vendor")


vendorServer :: Server Raw
vendorServer = Tagged $ staticApp $ embeddedSettings vendorFiles
