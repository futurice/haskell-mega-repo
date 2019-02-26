{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Futurice.App.Schedule.SchedulePdf where

import Futurice.Generics
import Futurice.IdMap    (Key)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Swagger         as Swagger

newtype SchedulePdf = SchedulePdf LBS.ByteString

instance ToSchema SchedulePdf where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "Pdf") $ mempty
        & Swagger.description ?~ "Pdf as raw data"

instance MimeRender OctetStream SchedulePdf where
    mimeRender c (SchedulePdf bs) = mimeRender c bs

generateSchedulePdf :: World -> (Key ScheduleTemplate) -> Maybe SchedulePdf
generateSchedulePdf w sid = let scheduleTemplate = w ^. worldScheduleTemplates . at sid --TODO implement pdf functionality
                            in Just $ SchedulePdf mempty
