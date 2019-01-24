{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Futurice.App.ProxyMgmt.Pages.Tokens (tokensPageHandler) where

import Control.Lens               (auf, coerced)
import Data.Coerce                (Coercible, coerce)
import Data.Semigroup             (Max (..), Option (..))
import Data.List                  (groupBy)
import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Generics          (textualToText)
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (cachedIO)
import Futurice.Time.Month        (dayToMonth)
import Prelude ()
import Servant.Links              (fieldLink)

import qualified Data.Map.Strict as Map

import Futurice.App.Proxy.API     (LenientEndpoint)

import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils

tokensPageHandler :: ReaderT (Login, Ctx f) IO (HtmlPage "tokens")
tokensPageHandler = do
    (_, ctx) <- ask
    liftIO $ do
        policies <- fetchPolicies ctx
        tokens <- fetchTokens ctx
        accessEntries <- fetchAccessEntries ctx
        currMonth <- currentMonth
        pure $ tokensPage currMonth policies tokens accessEntries

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

tokensPage :: Month -> [PolicyName] -> [Token] -> [AccessEntry] -> HtmlPage "tokens"
tokensPage currMonth policies tokens aes = page_ "Audit log" (Just NavTokens) $ do
    h2_ "New token"
    let fopts = FormOptions "add-token-form" (fieldLink routeAddToken) ("Add", "success")
    let policies' = fmap (\x -> (x, textualToText x)) policies
    lomakeHtml (Proxy @AddToken) fopts $
        vNothing :*
        vDynamic policies' :*
        Nil

    h2_ "Users + Tokens"
    sortableTable_ $ do
        thead_ $ tr_ $ do
            th_ "Username"
            th_ "Active"
            th_ "Last active"
            th_ "Policy"
            th_ "Accessed endpoints"
            th_ "Calls (per Month)"
        tbody_ $ for_ tokens $ \t -> tr_ $ do
            let ae = aes' ^. ix (tUserName t)
            td_ $ fromMaybe (toHtml $ tUserName t) $ do
                -- TODO: fetch personio data
                guard $ tUsertype t == "user"
                login <- parseLogin $ textualToText $ tUserName t
                pure $ toHtml login
            td_ $ if tActive t then "Active" else "Disabled"
            td_ $ traverse_ (toHtml . formatHumanHelsinkiTime) $ calaf (fmap Max . Option) foldMap (Just . aeStamp) ae
            td_ $ toHtml $ tPolicyName t
            let xs = calaf (UnionWith' . fmap Max) foldMap (\x -> Map.singleton (aeEndpoint x) (aeStamp x)) ae
            td_ $ ul_ $ ifor_ xs $ \e t -> li_ $ do
                toHtml e
                " → "
                toHtml (formatHumanHelsinkiTime t)
            let counts = map makeCounts . groupBy (eqBy aeEndpoint) $ filter isCurrMo ae
            td_ $ ul_ $ for_ counts $ \(e, c) -> li_ $ do
                toHtml e
                " → "
                toHtml (show c)
  where
    -- uses inlined DList
    aes' :: Map UserName [AccessEntry]
    aes' = Map.map ($[]) $ Map.fromListWith (.) $ map (\ae -> (aeUser ae, (ae :))) $ aes

    isCurrMo :: AccessEntry -> Bool
    isCurrMo = (== currMonth) . dayToMonth . utctDay . aeStamp

    makeCounts :: [AccessEntry] -> (LenientEndpoint, Int)
    makeCounts xs = (aeEndpoint $ head xs, length xs)

 -------------------------------------------------------------------------------
-- Util: move to futurice-prelude
-------------------------------------------------------------------------------

eqBy :: Eq b => (a -> b) -> a -> a -> Bool
eqBy f x y = f x == f y 

calaf :: forall f g a b. (Functor f, Functor g, Coercible a b)
      => (a -> b) -> (f b -> g b) -> f a -> g a
calaf _ = auf coerced

newtype UnionWith' k v = UnionWith' (Map k v)
instance (Ord k, Semigroup v) => Semigroup (UnionWith' k v) where
    UnionWith' a <> UnionWith' b = UnionWith' (Map.unionWith (<>) a b)
instance (Ord k, Semigroup v) => Monoid (UnionWith' k v) where
    mempty = UnionWith' Map.empty
    mappend = (<>)
