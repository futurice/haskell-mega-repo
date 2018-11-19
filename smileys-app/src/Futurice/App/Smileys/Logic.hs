{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Smileys.Logic (
    getSmileys,
    getOwnSmileys,
    postOwnSmileys,
    ) where

import FUM.Types.Login              (Login)
import Futurice.Postgres
import Futurice.Postgres.SqlBuilder
import Futurice.Prelude
import Prelude ()
import Servant                      (Handler, err403)

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
getSmileysImpl ctx s e mfu = safePoolQueryM ctx "smileys" $ do
    trail <- from_ "trail"
    fields_ trail [ "entries", "username", "smiley", "day" ]
    where_ trail "day" $ \day -> param1_ s [ day, ">= ?" ]
    where_ trail "day" $ \day -> param1_ e [ day, "<= ?" ]
    for_  mfu $ \fu ->
        where_ trail "username" $ \username -> param1_ fu [ username, " = ?" ]

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
