{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Badge.API where

import Codec.Picture             (DynamicImage)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.JuicyPixels       (PNG)

import qualified FUM.Types.Login as FUM

data Record route = Record
    { recIndex :: route :- SSOUser
        :> QueryParam "user" FUM.Login
        :> Get '[HTML] (HtmlPage "index-page")
    , recBadge :: route :- "badge" :> SSOUser
        :> Capture "user" FUM.Login
        :> Get '[PNG] DynamicImage
    }
  deriving Generic

type BadgeAPI = ToServantApi Record

badgeApi :: Proxy BadgeAPI
badgeApi = genericApi (Proxy :: Proxy Record)
