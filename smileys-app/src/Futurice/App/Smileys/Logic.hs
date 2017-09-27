{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Smileys.Logic (
    getSmileys,
    getOwnSmileys,
    postOwnSmileys,
    ) where

import FUM.Types.Login  (Login)
import Futurice.Prelude
import Prelude ()
import Servant          (Handler, err403)
import Futurice.Postgres

import Futurice.App.Smileys.Ctx
import Futurice.App.Smileys.Types

getOwnSmileys
    :: Ctx
    -> Maybe Login
    -> Maybe Day
    -> Maybe Day
    -> Handler [Smileys]
getOwnSmileys ctx mfum start end =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        liftIO $ runLogT "logic" (ctxLogger ctx) $ do
            today <- currentDay
            getSmileysImpl ctx (fromMaybe today start) (fromMaybe today end) (Just fumUsername)

getSmileys
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m)
    => Ctx
    -> Maybe Day
    -> Maybe Day
    -> Maybe Login
    -> m [Smileys]
getSmileys ctx start end mFumUsername =
    liftIO $ runLogT "logic" (ctxLogger ctx) $ do
        today <- currentDay
        getSmileysImpl ctx (fromMaybe today start) (fromMaybe today end) mFumUsername

getSmileysImpl
    :: Ctx
    -> Day
    -> Day
    -> Maybe Login
    -> LogT IO [Smileys]
getSmileysImpl ctx s e Nothing = safePoolQuery ctx
    "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ?;"
    (s, e)
getSmileysImpl ctx s e (Just fumUsername) = safePoolQuery ctx
    "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ? AND username = ?;"
    (s, e, fumUsername)

postOwnSmileys
    :: Ctx
    -> Maybe Login
    -> PostSmiley
    -> Handler Res
postOwnSmileys ctx mfum req =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        liftIO $ runLogT "logic" (ctxLogger ctx) $ do
            let insertQuery = fromString $ unwords
                     [ "INSERT INTO smileys.trail as c (entries, username, smiley, day)"
                     , "VALUES (?, ?, ?, ?) ON CONFLICT (username, day) DO UPDATE"
                     , "SET entries = EXCLUDED.entries, smiley = EXCLUDED.smiley"
                     ]

            _ <- safePoolExecute ctx insertQuery Smileys
                  { _smileysEntries  = _postSmileyEntries req
                  , _smileysUsername = fumUsername
                  , _smileysDate     = _postSmileyDate req
                  , _smileysSmiley   = _postSmileySmiley req
                  }

            pure Res
                { _resStatus = "OK"
                , _resUnused = ()
                }
