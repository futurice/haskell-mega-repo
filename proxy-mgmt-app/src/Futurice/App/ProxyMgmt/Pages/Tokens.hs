{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.Pages.Tokens (tokensPageHandler) where

import Control.Lens               (auf, coerced)
import Data.Coerce                (Coercible, coerce)
import Data.Semigroup             (Max (..), Option (..))
import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (cachedIO)
import Prelude ()

import qualified Data.Map.Strict as Map

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types

tokensPageHandler :: ReaderT (Login, Ctx f) IO (HtmlPage "tokens")
tokensPageHandler = do
    (_, ctx) <- ask
    liftIO $ do
        tokens <- fetchTokens ctx
        accessEntries <- fetchAccessEntries ctx
        pure $ tokensPage tokens accessEntries

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

fetchTokens :: Ctx f -> IO [Token]
fetchTokens Ctx {..} =
    runLogT "fetchTokens" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, passtext is not null, usertype, policyname FROM proxyapp.credentials ORDER BY username ASC;"

fetchAccessEntries :: Ctx f -> IO [AccessEntry]
fetchAccessEntries Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchAccessEntries" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE current_timestamp - updated < '6 months' :: interval ORDER BY updated DESC;"

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

tokensPage :: [Token] -> [AccessEntry] -> HtmlPage "tokens"
tokensPage tokens aes = page_ "Audit log" (Just NavTokens) $ do
    h2_ "Users + Tokens"
    sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Username"
            th_ "Active"
            th_ "Last active"
            th_ "Policy"
            th_ "Accessed endpoints"
        tbody_ $ for_ tokens $ \t -> tr_ $ do
            let ae = aes' ^. ix (tUsername t)
            td_ $ fromMaybe (toHtml $ tUsername t) $ do
                -- TODO: fetch personio data
                guard $ tUsertype t == "user"
                login <- parseLogin (tUsername t)
                pure $ toHtml login
            td_ $ if tActive t then "Active" else "Disabled"
            td_ $ traverse_ (toHtml . formatHumanHelsinkiTime) $ calaf (fmap Max . Option) foldMap (Just . aeStamp) ae
            td_ $ toHtml $ tPolicyName t
            let xs = calaf (UnionWith' . fmap Max) foldMap (\x -> Map.singleton (aeEndpoint x) (aeStamp x)) ae
            td_ $ ul_ $ ifor_ xs $ \e t -> li_ $ do
                toHtml e
                " → "
                toHtml (formatHumanHelsinkiTime t)
  where
    -- uses inlined DList
    aes' :: Map Text [AccessEntry]
    aes' = Map.map ($[]) $ Map.fromListWith (.) $ map (\ae -> (aeUser ae, (ae :))) $ aes

-------------------------------------------------------------------------------
-- Util: move to futurice-prelude
-------------------------------------------------------------------------------

calaf :: forall f g a b. (Functor f, Functor g, Coercible a b)
      => (a -> b) -> (f b -> g b) -> f a -> g a
calaf _ = auf coerced

newtype UnionWith' k v = UnionWith' (Map k v)
instance (Ord k, Semigroup v) => Semigroup (UnionWith' k v) where
    UnionWith' a <> UnionWith' b = UnionWith' (Map.unionWith (<>) a b)
instance (Ord k, Semigroup v) => Monoid (UnionWith' k v) where
    mempty = UnionWith' Map.empty
    mappend = (<>)
