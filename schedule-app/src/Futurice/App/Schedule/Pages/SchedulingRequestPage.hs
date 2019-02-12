{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Pages.SchedulingRequestPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Markup
import Futurice.App.Schedule.World

schedulingRequestPage :: World -> HtmlPage "scheduling-request-page"
schedulingRequestPage world = page_ "Scheduling Requests" (Just NavSchedulingRequest) $ do
    p_ ""
