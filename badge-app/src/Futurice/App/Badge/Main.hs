{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Badge.Main (defaultMain) where

import Codec.Picture
       (DynamicImage (ImageRGB8), convertRGB8, decodeImage)
import Data.List                   (find)
import Diagrams.Backend.Rasterific (rasterRgb8)
import Diagrams.Prelude            (dims2D)
import Futurice.Integrations       (Integrations, runIntegrations)
import Futurice.Lucid.Foundation   (HtmlPage, fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Cached              (getCached)
import Servant.Client
import Servant.Client.Generic      (genericClient)
import Servant.Server.Generic

import qualified Codec.Archive.Tar       as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified FUM.Types.Login         as FUM
import qualified Futurice.App.Avatar.API as Avatar
import qualified Personio                as P

import Futurice.App.Badge.API
import Futurice.App.Badge.Config
import Futurice.App.Badge.Ctx
import Futurice.App.Badge.Data
import Futurice.App.Badge.IndexPage
import Futurice.App.Badge.Templates
import Futurice.App.Badge.Tools

server :: Ctx -> Server BadgeAPI
server ctx = genericServer $ Record
    { recIndex = indexPageAction ctx
    , recBadge = badgePreviewAction ctx
    }

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index-page")
indexPageAction ctx a b = withAuthUser impl ctx a where
    impl fu = do
        return $ indexPage (fromMaybe fu b)

badgePreviewAction
    :: Ctx
    -> Maybe FUM.Login
    -> FUM.Login
    -> Handler DynamicImage
badgePreviewAction ctx mfu fu = withAuthUser' (error "foo") impl ctx mfu where
    impl _ = do
        employees <- P.personio P.PersonioEmployees
        return $ case find (\e -> e ^. P.employeeLogin == Just fu) employees of
            Nothing -> throwError err404
            Just e  -> do
                let imgAction = runClientM (avatarFum fu (Just Avatar.Original) False) cliEnv
                img <- liftIO $ do
                    contents <- imgAction >>= either throwM (pure . getResponse)
                    either
                        (throwM . ImageLoadingFailed "<avatar>")
                        (pure . convertRGB8)
                        (decodeImage $ LBS.toStrict $ getCached contents)

                 -- Picking template
                let template =
                        if e ^. P.employeeEmploymentType == Just P.Internal
                        then employeeTemplate
                        else externalTemplate

                -- Change `runTarM (ctxTar ctx)` to `liftIO` to tweak templates :)
                -- badge <- liftIO $
                badge <- runTarM (ctxTar ctx) $
                    template img
                        (e ^. P.employeeFirst . unpacked)
                        (e ^. P.employeeLast . unpacked)
                return $ ImageRGB8 $ rasterRgb8 (dims2D maxWidth maxHeight) badge

    Avatar.Record { Avatar.recFum = avatarFum } = genericClient
    cliEnv = mkClientEnv (ctxManager ctx) (cfgAvatarBaseurl $ ctxConfig ctx)

page404 :: HtmlPage a
page404 = page_ "Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService      .~ BadgeService
    & serverDescription  .~ "Tools for Badge"
    & serverApp badgeApi .~ server
    & serverColour       .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx       .~ "BADGEAPP"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr _cache _mq = do
        tar <- either fail pure $ removeTarErrors $ Tar.read tarContents
        let ctx = Ctx cfg lgr mgr tar
        pure (ctx, [])

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

withAuthUser
    :: (MonadIO m, MonadTime m)
    => (FUM.Login -> Integrations '[Proxy, Proxy, I, Proxy, Proxy, I] (HtmlPage a))
    -> Ctx -> Maybe FUM.Login
    -> m (HtmlPage a)
withAuthUser impl = withAuthUser' page404 (fmap return . impl)

withAuthUser'
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Integrations '[Proxy, Proxy, I, Proxy, Proxy, I] (m a))
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUser' def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        join $ liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
{-
            fus <- fum6 FUMGroupEmployees$ cfgAccessGroups $
            if fu `Set.notMember` fus
            then return def
            else action fu
-}
            action fu
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
