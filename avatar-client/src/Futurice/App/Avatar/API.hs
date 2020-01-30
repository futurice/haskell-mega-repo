{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar.API where

import Codec.Picture       (DynamicImage)
import Data.Swagger        (ToParamSchema (..))
import FUM.Types.Login     (Login)
import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.Cached
import Servant.JuicyPixels (PNG)

import qualified Data.Swagger as Swagger

data Record route = Record
    { recGeneric :: route :- "avatar"
        :> QueryParam' '[Required] "url" Text
        :> QueryParam "size" Size
        :> QueryFlag "grey"
        :> Get '[CACHED PNG] (Headers '[Header "Cache-Control" Text] (Cached PNG DynamicImage))
    , recFum :: route :- AvatarFumEndpoint
    }
  deriving Generic

type AvatarFumEndpoint = "fum"
    :> Capture "login" Login
    :> QueryParam "size" Size
    :> QueryFlag "grey"
    :> Get '[CACHED PNG] (Headers '[Header "Cache-Control" Text] (Cached PNG DynamicImage))

type AvatarAPI = ToServantApi Record

avatarApi :: Proxy AvatarAPI
avatarApi = genericApi (Proxy :: Proxy Record)

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

data Size
    = Original
    | Square Int
  deriving (Eq, Show)

instance FromHttpApiData Size where
    parseUrlPiece "original" = pure Original
    parseUrlPiece t = mkSquare <$> parseUrlPiece t

instance ToHttpApiData Size where
    toUrlPiece Original    = "original"
    toUrlPiece (Square n ) = toUrlPiece n

instance ToParamSchema Size where
    toParamSchema _ = mempty
        & Swagger.type_  .~ Swagger.SwaggerString
        & Swagger.format ?~ "original or number from 16 to 512"

mkSquare :: Int -> Size
mkSquare = Square . clamp 16 512 where
    clamp :: Ord a => a -> a -> a -> a
    clamp mi ma x
        | x < mi    = mi
        | x > ma    = ma
        | otherwise = x

fromMaybeSize :: Maybe Size -> Size
fromMaybeSize = maybe (Square 64) normalise where
    normalise Original = Original
    normalise (Square n) = mkSquare n
