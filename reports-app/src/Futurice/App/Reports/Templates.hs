{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.Templates (
    renderMustache,
    missingHoursEmailTemplate,
    missingHoursSmsTemplate,
    subcontractorEmailTemplate,
    subcontractorSmsTemplate,
    subcontractorHoursEmailTemplate,
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

subcontractorEmailTemplate :: Template
subcontractorEmailTemplate = either (error . show) id
    $ compileMustacheText "subcontractor-email.template"
    $(makeRelativeToProject "subcontractor-email.template" >>= embedStringFile)
{-# NOINLINE subcontractorEmailTemplate #-}

subcontractorSmsTemplate :: Template
subcontractorSmsTemplate = either (error . show) id
    $ compileMustacheText "subcontractor-sms.template"
    $(makeRelativeToProject "subcontractor-sms.template" >>= embedStringFile)
{-# NOINLINE subcontractorSmsTemplate #-}

subcontractorHoursEmailTemplate :: Template
subcontractorHoursEmailTemplate = either (error . show) id
    $ compileMustacheText "subcontractor-hours-email.template"
    $(makeRelativeToProject "subcontractor-hours-email.template" >>= embedStringFile)
{-# NOINLINE subcontractorHoursEmailTemplate #-}
