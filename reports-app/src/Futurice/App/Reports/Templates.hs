{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.Templates (
    renderMustache,
    missingHoursEmailTemplate,
    missingHoursSmsTemplate,
    subcontractorEmailTemplate,
    subcontractorSmsTemplate,
    subcontractorHoursEmailTemplate,
    returningEmployeeEmailTemplate,
    returningEmployeeEmailGermanyTemplate,
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

returningEmployeeEmailTemplate :: Template
returningEmployeeEmailTemplate = either (error . show) id
    $ compileMustacheText "returning-employee-email.template"
    $(makeRelativeToProject "returning-employee-email.template" >>= embedStringFile)

returningEmployeeEmailGermanyTemplate :: Template
returningEmployeeEmailGermanyTemplate = either (error . show) id
    $ compileMustacheText "returning-employee-email-germany.template"
    $(makeRelativeToProject "returning-employee-email-germany.template" >>= embedStringFile)
