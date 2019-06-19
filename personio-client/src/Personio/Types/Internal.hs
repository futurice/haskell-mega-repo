{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Personio.Types.Internal where

import Data.Aeson.Compat
import Data.Aeson.Types            (typeMismatch)
import Data.Fixed                  (Centi)
import Futurice.Aeson
import Futurice.CostCenter
import Futurice.Prelude
import Futurice.Time
import Prelude ()
import Text.Regex.Applicative.Text (RE', anySym, match, string)

import Personio.Internal.Attribute
import Personio.Types.EmployeeId

import qualified Data.Text                     as T
import qualified GitHub                        as GH
import qualified Text.Regex.Applicative.Common as RE

newtype SupervisorId = SupervisorId { getSupervisorId :: Maybe EmployeeId }

instance FromJSON SupervisorId where
    -- no supervisor: empty array
    parseJSON Null = pure (SupervisorId Nothing)
    parseJSON (Array xs) | null xs = pure (SupervisorId Nothing)
    parseJSON v = p v
      where
        p = withObjectDump "SupervisorId" $ \obj -> do
            type_ <- obj .: "type"
            if type_ == ("Employee" :: Text)
                then obj .: "attributes" >>= parseObject
                else fail $ "Attribute Supervisor is not Employee: " ++ type_ ^. unpacked

        parseObject :: Attributes -> Parser SupervisorId
        parseObject obj = SupervisorId <$> parseAttribute obj "id"

newtype NamedAttribute a = NamedAttribute { getName :: Maybe a }

instance FromJSON a => FromJSON (NamedAttribute a) where
    parseJSON v = case v of
        Null      -> pure (NamedAttribute Nothing)
        Array xs  -> case toList xs of
            []    -> pure (NamedAttribute Nothing)
            (x:_) -> p x  -- take first attribute.
        _         -> p v
      where
        p = withObjectDump "NamedAttribute" $ \obj ->
            NamedAttribute . Just <$> ((obj .: "attributes") >>= (.: "name"))

newtype GithubUsername = GithubUsername
    { getGithubUsername :: Maybe (GH.Name GH.User) }

instance FromJSON GithubUsername where
    parseJSON = withText "Github" $
        pure . GithubUsername . fmap GH.mkUserName . match githubRegexp

githubRegexp :: RE' Text
githubRegexp = string "https://github.com/" *> (T.pack <$> some anySym)

flowdockRegexp :: RE' Word64
flowdockRegexp = "https://" *> optional "www." *> "flowdock.com/app/private/" *> RE.decimal

newtype Expat = Expat { getExpat :: Bool }

instance FromJSON Expat where
    parseJSON = withText "Expat" $ \t -> pure . Expat $ case t of
        "Yes" -> True
        _     -> False  -- lenient

newtype WeeklyHours = WeeklyHours { getWeeklyHours :: NDT 'Hours Centi }

instance FromJSON WeeklyHours where
    parseJSON (String t) = case readMaybe (t ^. unpacked) of
        Nothing -> fail $ "Hours: " ++ show t
        Just x  -> pure (WeeklyHours (NDT x))
    parseJSON (Number n) = pure (WeeklyHours (realToFrac n))
    parseJSON v          = typeMismatch "WeeklyHours" v

data CostCenter' = CostCenter'
    { getCostCenter'          :: !CostCenter
    , getCostCenterPercentage :: !Double
    }

instance FromJSON CostCenter' where
    parseJSON = withObjectDump "Personio CostCenter" $ \obj -> do
        type_ <- obj .: "type"
        if type_ /= ("CostCenter" :: Text)
        then fail $ "Not cost center: " ++ type_ ^. unpacked
        else do
            attrs <- obj .: "attributes"
            CostCenter'
                <$> attrs .: "name"
                <*> attrs .: "percentage"
