{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.Templates (
    renderMustache,
    missingHoursEmailTemplate,
    missingHoursSmsTemplate,
    ) where

import Data.FileEmbed   (embedStringFile, makeRelativeToProject)
import Text.Microstache (Template, compileMustacheText, renderMustache)

missingHoursEmailTemplate :: Template
missingHoursEmailTemplate = either (error . show) id
    $ compileMustacheText "missing-hours-email.template"
    $(makeRelativeToProject "missing-hours-email.template" >>= embedStringFile)
{-# NOINLINE missingHoursEmailTemplate #-}

missingHoursSmsTemplate :: Template
missingHoursSmsTemplate = either (error . show) id
    $ compileMustacheText "missing-hours-sms.template"
    $(makeRelativeToProject "missing-hours-sms.template" >>= embedStringFile)
{-# NOINLINE missingHoursSmsTemplate #-}
