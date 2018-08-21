{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.EnvConfig (
    Configure (..),
    getConfig,
    getConfig',
    envVar,
    optionalEnvVar,
    envVarWithDefault,
    ConfigParser,
    FromEnvVar (..),
    FromEnvVarList (..),
    -- * Helpers
    envConnectInfo,
    envConnectInfo',
    envAwsCredentials,
    optionalAlt,
    -- * re-exports
    (<!>),
    ) where

import Algebra.Lattice
       (JoinSemiLattice (..), MeetSemiLattice (..))
import Control.Monad.Logger           (LogLevel (..))
import Data.Functor.Alt               (Alt (..))
import Data.List                      (foldl')
import Data.List.Split                (splitOn)
import Data.Semigroup.Foldable        (asum1)
import Database.PostgreSQL.Simple     (ConnectInfo (..))
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Futurice.Prelude
import Futurice.Time                  (NDT (..))
import Network.HTTP.Client            (Request, parseUrlThrow)
import Prelude ()
import Servant.Client                 (BaseUrl, parseBaseUrl)
import System.Environment             (getEnvironment)
import System.Exit                    (exitFailure)

import qualified Chat.Flowdock.REST as FD
import qualified Data.ByteString    as B
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T
import qualified Data.UUID.Types    as UUID
import qualified GitHub             as GH
import qualified Network.AWS        as AWS

data EnvVarP a = EnvVar
    { _envVarName :: String
    , _envVarP    :: String -> Maybe a
    }

-- | Get the configuration from environment variables.
getConfig :: (MonadLog m, MonadIO m, Configure cfg)
    => String  -- ^ envvar prefix
    -> m cfg
getConfig pfx = getConfig' pfx configure

-- | Explicit version of 'getConfig'
getConfig' :: (MonadLog m, MonadIO m) => String -> ConfigParser cfg -> m cfg
getConfig' pfx cp = logLocalDomain "env-config" $ do
    env <- Map.fromList <$> liftIO getEnvironment
    let v = runCP (\x -> f "" env x <!> f (pfx <> "_") env x) cp
    case v of
        Success a -> pure a
        Failure (CNF cs) -> do
            for_ cs $ \ds ->
                logAttention_ $ T.intercalate " || " $ toList ds
            liftIO exitFailure
  where
    f :: String -> Map String String -> EnvVarP a -> Validation CNF a
    f pfx' env (EnvVar name p) = eitherToValidation $ do
        let name' = pfx' <> name
        x <- lookupEnvE name' env
        maybe (Left $ cnf $ "Invalid " <> name' <> "=" <> x) Right (p x)

    lookupEnvE name
        = maybe (Left $ cnf $ name <> " not set") Right
        . Map.lookup name

singleton :: a -> NonEmpty a
singleton x = x :| []

-------------------------------------------------------------------------------
-- Aliases and classes
-------------------------------------------------------------------------------

type ConfigParser = CP EnvVarP

class Configure cfg where
    configure :: ConfigParser cfg

instance Configure () where
    configure = pure ()

-- | Class to parse env variables
class FromEnvVar a where
    fromEnvVar :: String -> Maybe a

class FromEnvVarList a where
    fromEnvVarList :: String -> Maybe [a]

instance FromEnvVarList a => FromEnvVar [a] where
    fromEnvVar = fromEnvVarList

instance (FromEnvVarList a, Ord a) => FromEnvVar (Set a) where
    fromEnvVar = fmap Set.fromList . fromEnvVarList

-------------------------------------------------------------------------------
-- CP
-------------------------------------------------------------------------------

-- | This is eseentially 'Control.Alternative.Free.Alt', but with 'NonEmpty'.
--
-- We have abstract free struture to ensure correctness
newtype CP f a = Alt (NonEmpty (CP' f a))

runCP :: forall f g a. (Applicative g, Alt g) => (forall x. f x -> g x) -> CP f a -> g a
runCP u = go
  where
    go :: forall b. CP f b -> g b
    go (Alt xs) = asum1 (fmap go' xs)

    go' :: forall b. CP' f b -> g b
    go' (Pure x) = pure x
    go' (Ap x f) = flip id <$> u x <*> go f

liftCP :: f a -> CP f a
liftCP x = Alt $ singleton $ Ap x (pure id)

instance Functor (CP f) where
    fmap f (Alt xs) = Alt (fmap (fmap f) xs)

instance Applicative (CP f) where
    pure = Alt . singleton . pure

    Alt xs <*> Alt ys = Alt $ (<*>) <$> xs <*> ys

instance Alt (CP f) where
    Alt a <!> Alt b = Alt (a <> b)


data CP' f a where
    Ap     :: f b -> CP f (b -> a) -> CP' f a
    Pure   :: a                    -> CP' f a

instance Functor (CP' f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Ap x g) = Ap x (fmap (f .) g)

instance Applicative (CP' f) where
    pure = Pure

    Pure f <*> x = fmap f x     -- fmap
    y <*> Pure a = fmap ($ a) y -- interchange
    Ap x f <*> y = Ap x (flip <$> f <*> Alt (singleton y))

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

envVar :: FromEnvVar a => String -> ConfigParser a
envVar name = liftCP (EnvVar name fromEnvVar)

optionalEnvVar :: FromEnvVar a => String -> ConfigParser (Maybe a)
optionalEnvVar name = Just <$> envVar name <!> pure Nothing

envVarWithDefault :: FromEnvVar a => String -> a -> ConfigParser a
envVarWithDefault name d = envVar name <!> pure d

envConnectInfo :: ConfigParser ConnectInfo
envConnectInfo = envConnectInfo' ""

envConnectInfo' :: String -> ConfigParser ConnectInfo
envConnectInfo' pfx = old <!> new
  where
    f connInfo password = connInfo { connectPassword = password }

    old = f
        <$> envVar (pfx ++ "POSTGRES_URL")
        <*> envVar (pfx ++ "POSTGRES_PASS")

    new = envVar (pfx ++ "DB_URL")

envAwsCredentials :: String -> ConfigParser AWS.Credentials
envAwsCredentials pfx = AWS.FromKeys
    <$> envVar (pfx ++ "ACCESSKEY")
    <*> envVar (pfx ++ "SECRETKEY")

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance FromEnvVarList Char where
    fromEnvVarList = Just

instance FromEnvVarList Int where
    fromEnvVarList =  traverse fromEnvVar . splitOn ","

instance FromEnvVar Text where
    fromEnvVar = Just . view packed

instance FromEnvVar Int where
    fromEnvVar = readMaybe

instance FromEnvVar B.ByteString where
    fromEnvVar = Just . fromString

instance FromEnvVar Word64 where
    fromEnvVar = readMaybe

instance FromEnvVar ConnectInfo where
    fromEnvVar = parseDatabaseUrl

instance FromEnvVar LogLevel where
    fromEnvVar "DEBUG" = Just LevelDebug
    fromEnvVar "INFO"  = Just LevelInfo
    fromEnvVar "WARN"  = Just LevelWarn
    fromEnvVar "ERROR" = Just LevelError
    fromEnvVar _       = Nothing

instance FromEnvVar Bool where
    fromEnvVar "1"   = Just True
    fromEnvVar "YES" = Just True
    fromEnvVar "0"   = Just False
    fromEnvVar "NO"  = Just False
    fromEnvVar _     = Nothing

instance FromEnvVar Request where
    -- TODO: change to parseRequest
    fromEnvVar s = fromEnvVar s >>= parseUrlThrow

instance FromEnvVar BaseUrl where
    fromEnvVar s = fromEnvVar s >>= parseBaseUrl

-------------------------------------------------------------------------------
-- Futurice.Time
-------------------------------------------------------------------------------

instance FromEnvVar a => FromEnvVar (NDT u a) where
    fromEnvVar = fmap NDT . fromEnvVar

-------------------------------------------------------------------------------
-- AWS
-------------------------------------------------------------------------------

instance FromEnvVar AWS.AccessKey where
    fromEnvVar = fmap AWS.AccessKey . fromEnvVar

instance FromEnvVar AWS.SecretKey where
    fromEnvVar = fmap AWS.SecretKey . fromEnvVar

-------------------------------------------------------------------------------
-- GitHub
-------------------------------------------------------------------------------

instance FromEnvVar GH.Auth where
    fromEnvVar = fmap GH.OAuth . fromEnvVar

instance FromEnvVar (GH.Name a) where
    fromEnvVar = fmap (GH.mkName Proxy) . fromEnvVar

-------------------------------------------------------------------------------
-- Flowdock
-------------------------------------------------------------------------------

instance FromEnvVar FD.AuthToken where
    fromEnvVar = fmap FD.AuthToken . fromEnvVar

instance FromEnvVar (FD.ParamName a) where
    fromEnvVar = fmap FD.mkParamName . fromEnvVar

-------------------------------------------------------------------------------
-- UUID
-------------------------------------------------------------------------------

instance FromEnvVar UUID.UUID where
    fromEnvVar = UUID.fromString

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | 'Validation' is 'Either' with a Left that is a 'Lattice'
data Validation e a
    = Failure e
    | Success a
    deriving (Eq, Ord, Show)

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance MeetSemiLattice e => Applicative (Validation e) where
    pure = Success
    Failure e1 <*> Failure e2 = Failure (e1 /\ e2)
    Failure e1 <*> Success _  = Failure e1
    Success _  <*> Failure e2 = Failure e2
    Success f  <*> Success a  = Success (f a)

instance JoinSemiLattice e => Alt (Validation e) where
    Failure e1 <!> Failure e2 = Failure (e1 \/ e2)
    Failure _  <!> Success a  = Success a
    Success a  <!> _          = Success a

eitherToValidation :: Either e a -> Validation e a
eitherToValidation x = case x of
    Left e  -> Failure e
    Right a -> Success a
{-# INLINE eitherToValidation #-}

-------------------------------------------------------------------------------
-- CNF
-------------------------------------------------------------------------------

newtype CNF = CNF (Set (Set Text))

cnf :: String -> CNF
cnf = CNF . Set.singleton . Set.singleton . T.pack

instance MeetSemiLattice CNF where
    CNF a /\ CNF b = CNF (a <> b)

instance JoinSemiLattice CNF where
    CNF a \/ CNF b = CNF $ optimise $ [ a' <> b' | a' <- toList a, b' <- toList b]

optimise :: [Set Text] -> Set (Set Text)
optimise zs = foldl' f (Set.fromList zs) ms
  where
    ms :: [Set Text]
    ms = sortOn Set.size zs

    f :: Set (Set Text) -> Set Text -> Set (Set Text)
    f xs y = Set.map (g y) xs

    g y x
        | y `Set.isProperSubsetOf` x = y
    g _ x = x

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Like 'optional' but for 'Alt', not 'Alternative'
optionalAlt :: (Applicative f, Alt f) => f a -> f (Maybe a)
optionalAlt x = Just <$> x <!> pure Nothing
