{-# LANGUAGE OverloadedStrings #-}
module Futurice.Signed (
    Signed (..),
    sign,
    cosign,
    basicPublicKey,
    -- * Re-exports
    Ed.PublicKey,
    Ed.SecretKey,
    Ed.Signature,
    ) where

import Data.Aeson
       (FromJSON (..), ToJSON (..), Value (..), object, pairs, withObject,
       (.:), (.=))
import Data.Typeable    (typeRep)
import Futurice.Prelude
import Prelude ()

import qualified Codec.Serialise            as S
import qualified Crypto.Hash.SHA512         as SHA512
import qualified Crypto.Sign.Ed25519        as Ed
import qualified Data.Aeson.Encoding        as AesonE
import qualified Data.ByteString.Base16     as Base16
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.Map                   as Map
import qualified Data.Swagger               as Sw

-- | Signed data.
--
-- Signatures are calculated from SHA512 hash of 'Serialise'd binary,
-- which makes it technically possible to verify them from other than
-- Hasksell clients (it's CBOR); but quite difficult still.
--
data Signed a = Signed
    { signed     :: a
    , signatures :: Map Ed.PublicKey Ed.Signature
    }
  deriving (Eq, Show)

-- | A public key of "basic" key.
basicPublicKey :: Ed.PublicKey
basicPublicKey = Ed.PublicKey $ fst $ Base16.decode
    "5d0648c974515e3829d8149e9c60b3fb9a0bdef82fe4c74ea0ea657d6f02a3a3"

-- | Sign piece of data.
--
-- >>> sign [sk] [1,2,3 :: Int]
-- Signed {signed = [1,2,3], signatures = fromList [(PublicKey {unPublicKey = "-\ENQs...
--
-- >>> LBS8.putStrLn $ Aeson.encode $ Aeson.toJSON $ sign [sk] [1,2,3 :: Int]
-- {"signed":[1,2,3],"signatures":{"2d0573beebecb4cf83b02cb4a95ce78da1885c8757ffb39e4cf74f754a1ebc24":"c9b4494d60e5d1b383e02ed080d08e89fa972e72238eac409d795866231e9ec964109e6779d5518d4a76be859f3fbda3a5d111311cc7d45f3ba4d6977155bd09"}}
--
-- >>> LBS8.putStrLn $ Aeson.encode $ sign [sk] [1,2,3 :: Int]
-- {"signed":[1,2,3],"signatures":{"2d0573beebecb4cf83b02cb4a95ce78da1885c8757ffb39e4cf74f754a1ebc24":"c9b4494d60e5d1b383e02ed080d08e89fa972e72238eac409d795866231e9ec964109e6779d5518d4a76be859f3fbda3a5d111311cc7d45f3ba4d6977155bd09"}}
--
-- >>> LBS8.putStrLn $ Aeson.encode $ sign [sk, sk2] [1,2,3 :: Int]
-- {"signed":[1,2,3],"signatures":{"2d0573beebecb4cf83b02cb4a95ce78da1885c8757ffb39e4cf74f754a1ebc24":"c9b4494d60e5d1b383e02ed080d08e89fa972e72238eac409d795866231e9ec964109e6779d5518d4a76be859f3fbda3a5d111311cc7d45f3ba4d6977155bd09","bce6133c0aa2b5fa593d8df5b9569749516ed764467f5b5a8d5ea3b042b2f6f1":"01fbe3a85920a2b2ce9c3159b624c4c9f3327e6112b3a3fed2820991e281817a5983d69e376c54505c3bb09f8ff9e8215cf453e431bea03833555bd62a4cbd05"}}
--
-- >>> Aeson.decode $ Aeson.encode $ sign [sk] [1,2,3 :: Int] :: Maybe (Signed [Int])
-- Just (Signed {signed = [1,2,3], signatures = fromList [(PublicKey {unPublicKey = "-\ENQs...
--
-- >>> Aeson.eitherDecode $ Aeson.encode $ sign [sk] [1,2,3 :: Int] :: Either String (Signed [Double])
-- Left "Error in $: Wrong signature. Key: \"2d0573beebecb4cf83b02cb4a95ce78da1885c8757ffb39e4cf74f754a1ebc24\""
--
sign :: S.Serialise a => [Ed.SecretKey] -> a -> Signed a
sign sks x = Signed
    { signed     = x
    , signatures = Map.fromList
        [ (Ed.toPublicKey sk, Ed.dsign sk msg)
        | sk <- toList sks
        ]
    }
  where
    msg = SHA512.hashlazy $ S.serialise x

-- | Co-sign data with more keys.
--
-- >>> sign [sk, sk2] [1,2,3 :: Int] == cosign [sk2] (sign [sk] [1,2,3])
-- True
--
cosign :: S.Serialise a => [Ed.SecretKey] -> Signed a -> Signed a
cosign sks (Signed x ss) = Signed
    { signed = x
    , signatures = ss <> Map.fromList
        [ (Ed.toPublicKey sk, Ed.dsign sk msg)
        | sk <- toList sks
        ]
    }
  where
    msg = SHA512.hashlazy $ S.serialise x

instance ToJSON a => ToJSON (Signed a) where
    toJSON (Signed x ss) = object
        [ "signed" .= x
        , "signatures" .= Object (HM.fromList
            [ (bsToText k, String (bsToText s))
            | (Ed.PublicKey k, Ed.Signature s) <- Map.toList ss
            ])
        ]
    toEncoding (Signed x ss) = pairs $
        ("signed" .= x) <>
        AesonE.pair "signatures" (AesonE.dict AesonE.text AesonE.text
            (\f z xs -> foldr (\(Ed.PublicKey p, Ed.Signature s) -> f (bsToText p) (bsToText s)) z xs)
            (Map.toList ss))

instance (FromJSON a, S.Serialise a) => FromJSON (Signed a) where
    parseJSON = withObject "Signed" $ \obj -> do
        x <- obj .: "signed"
        m <- obj .: "signatures"

        let ss = textToSignatures m
        let msg = SHA512.hashlazy $ S.serialise x

        ifor_ ss $ \p@(Ed.PublicKey p') s ->
            if Ed.dverify p msg s
            then pure ()
            else fail $ "Wrong signature. Key: " ++ show (Base16.encode p')

        return (Signed x ss)

instance (Sw.ToSchema a, Typeable a) => Sw.ToSchema (Signed a) where
    declareNamedSchema _ = do
        let name = textShow $ typeRep (Proxy :: Proxy a)
        ref <- Sw.declareSchemaRef (Proxy :: Proxy a)
        return $ Sw.NamedSchema (Just $ "Signed " <> name) $ mempty
            & Sw.type_ .~ Just Sw.SwaggerObject
            & Sw.properties .~ InsOrd.fromList
                [ ("signed", ref)
                -- TODO: add signatures
                ]
            & Sw.required .~ [ "signed" ]

bsToText :: ByteString -> Text
bsToText = decodeUtf8Lenient .  Base16.encode

textToBs :: Text -> ByteString
textToBs = fst . Base16.decode . encodeUtf8

textToSignatures :: HashMap Text Text -> Map Ed.PublicKey Ed.Signature
textToSignatures tt = Map.fromList
    [ (Ed.PublicKey (textToBs p), Ed.Signature (textToBs s))
    | (p, s) <- HM.toList tt
    ]

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> (pk, sk)   <- maybe (error "create-keypair") return $ Ed.createKeypairFromSeed_ "deadcode12345678qwertyui12345678"
-- >>> (pk2, sk2) <- maybe (error "create-keypair") return $ Ed.createKeypairFromSeed_ "deadcode12345678qwertyuiasdfghjk"
