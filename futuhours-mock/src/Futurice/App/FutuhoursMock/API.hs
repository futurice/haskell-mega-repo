{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.FutuhoursMock.Types
import Servant

type FutuhoursV1API =
      ( "projects" :> Get '[JSON] (Vector Project)
        :<|> "user" :> Get '[JSON] (User) -- TODO: should return logged-in User information
        :<|> "hours" :> QueryParam "start-date" Day
                     :> QueryParam "end-date" Day
                     :> Get '[JSON] (HoursResponse)
        :<|> "entry" :> ReqBody '[JSON] EntryUpdate :> Post '[JSON] (EntryUpdateResponse)
        :<|> "entry" :> Capture "id" Int :> ReqBody '[JSON] EntryUpdate :> Put '[JSON] (EntryUpdateResponse)
        :<|> "entry" :> Capture "id" Int :> ReqBody '[JSON] EntryUpdate :> Delete '[JSON] (EntryUpdateResponse)
      )

type FutuhoursAPI = Get '[JSON] Text
    :<|> "api" :> "v1" :> FutuhoursV1API

futuhoursApi :: Proxy FutuhoursAPI
futuhoursApi = Proxy
