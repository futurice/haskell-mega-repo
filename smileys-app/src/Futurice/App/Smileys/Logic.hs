{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.Smileys.Logic (getSmileys, postSmiley) where

import Prelude ()
import Futurice.Prelude
import Data.Pool        (withResource)

import Futurice.App.Smileys.Types
import Futurice.App.Smileys.Ctx

import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM
import Servant (ServantErr(..), err403)

getSmileys
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m, MonadError ServantErr m)
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> m [Smileys]
getSmileys ctx mfum start end =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        withResource (ctxPostgresPool ctx) $ \conn -> do
            today <- currentDay
            q conn fumUsername (fromMaybe today start) (fromMaybe today end)
          where
            q :: (MonadIO m, MonadBaseControl IO m)
              => Postgres.Connection -> FUM.UserName -> Day -> Day -> m [Smileys]
            q conn fumUsername s e = liftIO $ Postgres.query conn
                "SELECT entries, username, smiley, day FROM smileys.trail WHERE day >= ? AND day <= ? AND username = ?;"
                (s, e, fumUsername)

postSmiley
    :: (MonadIO m, MonadBaseControl IO m, MonadError ServantErr m)
    => Ctx
    -> Maybe FUM.UserName
    -> PostSmiley
    -> m Res
postSmiley ctx mfum req =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername ->
        withResource (ctxPostgresPool ctx) $ \conn -> do
            let insertQuery = fromString $ unwords $
                 [ "INSERT INTO smileys.trail as c (entries, username, smiley, day)"
                 , "VALUES (?, ?, ?, ?) ON CONFLICT (username, day) DO UPDATE"
                 , "SET entries = EXCLUDED.entries, smiley = EXCLUDED.smiley"
                 ]

            _ <- liftIO $ Postgres.execute conn insertQuery Smileys
                  { _smileysEntries  = _postSmileyEntries req
                  , _smileysUsername = fumUsername
                  , _smileysDate     = _postSmileyDate req
                  , _smileysSmiley   = _postSmileySmiley req
                  }
            pure $ Res { _resStatus = "OK" }
