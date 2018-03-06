{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.AdminPage (adminPageHandler) where

import Control.Lens                       (auf, coerced)
import Data.Coerce                        (Coercible, coerce)
import Data.Semigroup                     (Max (..), Option (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import FUM.Types.Login
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant                   (cachedIO)
import Prelude ()

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types

import qualified Data.Map.Strict as Map

adminPageHandler :: Ctx f -> ReaderT Login IO (HtmlPage "admin")
adminPageHandler ctx = liftIO $ do
    tokens <- fetchTokens ctx
    accessEntries <- fetchAccessEntries ctx
    audit <- fetchAudit ctx
    pure $ adminPage tokens accessEntries audit

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

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

fetchTokens :: Ctx f -> IO [Token]
fetchTokens Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchTokens" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, passtext is not null, usertype, endpoint FROM proxyapp.credentials ORDER BY username ASC;"

fetchAccessEntries :: Ctx f -> IO [AccessEntry]
fetchAccessEntries Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchAccessEntries" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE current_timestamp - updated < '6 months' :: interval ORDER BY updated DESC;"

fetchAudit :: Ctx f -> IO [(Login, UTCTime, Text)]
fetchAudit Ctx {..} = 
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchAudit" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, created, message FROM proxyapp.auditlog ORDER BY created DESC;"

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

adminPage :: [Token] -> [AccessEntry] -> [(Login, UTCTime, Text)] -> HtmlPage "admin"
adminPage tokens aes audit = page_ "Admin" (Just NavAdmin) $ do
    div_ [ class_ "callout warning" ] $ do
        "TODO"
        ul_ $ do
            li_ "Management in general as: Create token"
            li_ "Disable token"
            li_ "Add / disable endpoint for token"

    h2_ "Users + Tokens"
    sortableTable_ $ do
        thead_ $ do
            th_ "Username"
            th_ "Active"
            th_ "Last active"
            th_ "Endpoint"
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
            td_ $ toHtml $ tEndpoint t
            let xs = calaf (UnionWith' . fmap Max) foldMap (\x -> Map.singleton (aeEndpoint x) (aeStamp x)) ae
            td_ $ ul_ $ ifor_ xs $ \e t -> li_ $ do
                toHtml e
                " → "
                toHtml (formatHumanHelsinkiTime t)

    h2_ "Audit log"
    sortableTable_ $ do
        thead_ $ do
            th_ "Username"
            th_ "Timestamp"
            th_ "Message"
        tbody_ $ for_ audit $ \(login, stamp, msg) -> tr_ $ do
            td_ $ toHtml login
            td_ $ toHtml . formatHumanHelsinkiTime $ stamp
            td_ $ toHtml msg
  where
    -- uses inlined DList
    aes' :: Map Text [AccessEntry]
    aes' = Map.map ($[]) $ Map.fromListWith (.) $ map (\ae -> (aeUser ae, (ae :))) $ aes
