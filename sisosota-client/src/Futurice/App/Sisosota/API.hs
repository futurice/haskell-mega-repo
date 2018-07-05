{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Sisosota.API where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic

import Futurice.App.Sisosota.Types

data Record route = Record
    { recGet :: route :- "get"
        :> Summary "Retrieve file"
        :> Capture' '[Description "Content hash"] "hash" ContentHash
        :> Get '[OctetStream] ContentData
    , recPut :: route :- "put"
        :> Summary "Store file"
        :> ReqBody' '[Description "Content data"] '[OctetStream] ContentData
        :> Put '[JSON] ContentHash
    }
  deriving Generic

type SisosotaAPI = ToServantApi Record

sisosotaApi :: Proxy SisosotaAPI
sisosotaApi = genericApi (Proxy :: Proxy Record)
