{-# LANGUAGE DataKinds #-}
module Futurice.App.Contacts.Config (
    Config,
    ) where

import Futurice.Integrations
import Futurice.Prelude ()
import Prelude ()

type Config = IntegrationsConfig '[ ServFUM, ServGH, ServPE, ServPM, ServPO ]
