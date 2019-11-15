{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.Pages.Index (indexPageHandler) where

import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Constants         (servicePublicUrl, supportEmailHtml)
import Futurice.Generics
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (Service (ProxService), cachedIO)
import Prelude ()

import Futurice.App.Proxy.API        (LenientEndpoint (..))
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils
       (fetchPolicyEndpoints, fetchServiceTokensCreatedBy)

indexPageHandler :: ReaderT (Login, Ctx) IO (HtmlPage "index")
indexPageHandler = ReaderT $ \(login, ctx) -> do
    mtoken <- fetchToken login ctx
    entries <- fetchAccessEntries login ctx
    policyEndpoints <- fetchPolicyEndpoints ctx
    createdTokens <- fetchServiceTokensCreatedBy ctx login
    return $ indexPage login mtoken entries policyEndpoints createdTokens

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

fetchToken :: Login -> Ctx -> IO (Maybe Token)
fetchToken login Ctx {..} =
    cachedIO ctxLogger ctxCache 600 login $ runLogT "fetchTokens" ctxLogger $
        listToMaybe <$> safePoolQuery ctxPostgresPool
            "SELECT username, passtext is not null, usertype, policyname FROM proxyapp.credentials WHERE username = ?;"
            (Only login)

fetchAccessEntries :: Login -> Ctx -> IO [AccessEntry]
fetchAccessEntries login Ctx {..} =
    cachedIO ctxLogger ctxCache 600 login $ runLogT "fetchAccessEntries" ctxLogger $ do
        safePoolQuery ctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE username = ? ORDER BY updated DESC LIMIT 100;"
            (Only login)

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

indexPage :: Login -> Maybe Token -> [AccessEntry] -> Map PolicyName (Set LenientEndpoint) -> [Token] -> HtmlPage "index"
indexPage login mtoken entries policyEndpoints createdTokens =
    maybe (noTokenPage login) (tokenPage login entries policyEndpoints createdTokens) mtoken

noTokenPage :: Login -> HtmlPage "index"
noTokenPage login =
    page_ ("Prox management - " <> loginToText login <> " - no token") (Just NavIndex) $ do
        p_ $ do
            "You don't have active prox token. "
            "Contact IT at "
            supportEmailHtml
            ", if you need one."

        p_ $
            "What is prox? TBW"

tokenPage :: Login -> [AccessEntry] ->  Map PolicyName (Set LenientEndpoint) -> [Token] -> Token -> HtmlPage "index"
tokenPage login entries policyEndpoints createdTokens token = page_ ("Prox management - " <> loginToText login) (Just NavIndex) $ do
    h2_ "Token"
    condensedTable_ $ tbody_ $ do
        vertRow_ "Active" $ if tActive token then "Active" else "Passive"
        vertRow_ "Policy" $ toHtml $ tPolicyName token
        vertRow_ "Endpoint (prefixes)" $ ul_ $ for_ (policyEndpoints ^.. ix (tPolicyName token) . folded) $ \(LenientEndpoint endpoint) ->
            li_ $ a_ [ href_ $ servicePublicUrl ProxService <> endpoint ] $ toHtml endpoint

    h2_ "Regenerate token"
    p_ "If you’ve lost or forgotten the token, you can regenerate it, but be aware that any scripts or applications using this token will need to be updated."
    p_ $ button_ [ id_ "futu-regenerate-token", class_ "button alert", disabled_ "disabled" ] "Regenerate"
    div_ [ id_ "futu-token-info", class_ "callout success", style_ "display: none" ] $ do
        p_ "Make sure to copy your new personal access token now. You won’t be able to see it again!"
        condensedTable_ $ tbody_ $
            vertRow_ "new token" $
                code_ [ id_ "futu-token-code" ] "01234567890abcdef01234567890abcd"

    h2_ "Regenerate service token"
    p_ "Regenerate service user's tokens that you have created. Same rules as with our own token"
    div_ [ id_ "futu-service-token-info", class_ "callout success", style_ "display: none" ] $ do
        p_ "Make sure to copy service user access token now. You won’t be able to see it again!"
        condensedTable_ $ tbody_ $ do
            vertRow_ "service user" $ span_ [ id_ "futu-service-token-user"] $ "somebody"
            vertRow_ "new token" $
                code_ [ id_ "futu-service-token-code" ] "01234567890abcdef01234567890abcd"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Service user"
            th_ ""
        tbody_ $
            for_ createdTokens $ \token -> tr_ $ do
                td_ $ toHtml $ tUserName token
                td_ $ button_ [ id_ "futu-regenerate-service-token"
                              , class_ "button alert"
                              , disabled_ "disabled"
                              , data_ "service-username" (textualToText $ tUserName token)] "Regenerate"


    h2_ "Last 100 accesses"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Timestamp"
            th_ "Endpoint"

        tbody_ $ for_ entries $ \AccessEntry {..} -> tr_ $ do
            td_ $ toHtml $ show aeStamp
            td_ $ toHtml aeEndpoint
