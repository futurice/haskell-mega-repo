{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar.API where

import Codec.Picture       (DynamicImage)
import FUM.Types.Login     (Login)
import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.JuicyPixels (PNG)

type AvatarAPI =
    Get '[PlainText] Text
    :<|> GenericAvatar
    :<|> FumAvatar

type GenericAvatar = "avatar"
    :> QueryParam' '[Required] "url" Text
    :> QueryParam "size" Int
    :> QueryFlag "grey"
    :> Get '[PNG] (Headers '[Header "Cache-Control" Text] DynamicImage)

type FumAvatar = "fum"
    :> Capture "login" Login
    :> QueryParam "size" Int
    :> QueryFlag "grey"
    :> Get '[PNG] (Headers '[Header "Cache-Control" Text] DynamicImage)

avatarApi :: Proxy AvatarAPI
avatarApi = Proxy

fumAvatarEndpoint :: Proxy FumAvatar
fumAvatarEndpoint = Proxy
