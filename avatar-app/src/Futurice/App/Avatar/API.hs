{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar.API where

import Futurice.Prelude
import Prelude ()

import Codec.Picture       (DynamicImage)
import Servant
import Servant.JuicyPixels (PNG)

type AvatarAPI =
    Get '[PlainText] Text
    :<|> GenericAvatar

type GenericAvatar = "avatar"
    :> QueryParam' '[Required] "url" Text
    :> QueryParam "size" Int
    :> QueryFlag "grey"
    :> Get '[PNG] (Headers '[Header "Cache-Control" Text] DynamicImage)

avatarApi :: Proxy AvatarAPI
avatarApi = Proxy
