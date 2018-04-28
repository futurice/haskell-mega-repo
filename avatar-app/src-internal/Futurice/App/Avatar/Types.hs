{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.Avatar.Types where

import Futurice.Aeson
import Futurice.Prelude
import Prelude ()

import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.Binary                as Binary
import qualified Data.ByteString.Base64.URL as Base64

data AvatarProcess = AvatarProcess
    { apUrl  :: !Text
    , apSize :: !Int
    , apGray :: !Bool
    }
  deriving (Show, Generic)

instance Binary AvatarProcess

avatarProcessDigest :: AvatarProcess -> Text
avatarProcessDigest
    = decodeUtf8Lenient
    . Base64.encode
    . SHA256.hashlazy
    . Binary.encode

instance FromJSON AvatarProcess where
    parseJSON = withObjectDump "AvatarProcess" $ \obj -> AvatarProcess
        <$> obj .: "url"
        <*> obj .: "size"
        <*> obj .: "gray"

instance ToJSON AvatarProcess where
    toJSON AvatarProcess {..} = object $
        [ "url"  .= apUrl
        , "size" .= apSize
        , "gray" .= apGray
        ]
