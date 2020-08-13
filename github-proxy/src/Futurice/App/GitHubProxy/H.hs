{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Futurice.App.GitHubProxy.H (
    H, runH,
    ) where

import Control.Monad.Operational     (Program, interpretWithMonad, singleton)
import Data.Aeson                    (FromJSON, object)
import Futurice.GitHub               (requestToJSON)
import Futurice.Integrations.Classes (MonadGitHub (..))
import Futurice.Metrics.RateMeter    (mark)
import Futurice.Prelude
import GitHub.Auth                   (Auth)
import Prelude ()

import qualified GitHub as GH

data R a where
    R :: (NFData a, FromJSON a) => GH.Request 'GH.RA a -> R a

newtype H a = H { unH :: Program R a }

instance Functor H where
    fmap f (H x) = H (fmap f x)

instance Applicative H where
    pure = H . pure
    H f <*> H x = H (f <*> x)
    H f *> H x = H (f *> x)

instance Monad H where
    return = pure
    (>>) = (*>)
    H f >>= k = H $ f >>= unH . k

instance MonadGitHub H where
    type MonadGitHubC H = NFData
    githubReq req = H (singleton (R req))

runH :: Logger -> Auth -> H a -> IO a
runH logger auth (H m) = do
    mgr <- newManager tlsManagerSettings
    interpretWithMonad (interpret mgr) m
  where
    interpret :: Manager -> R x -> IO x
    interpret mgr (R req) = runLogT "github" logger $ do
        logTrace "Request" $ object $ requestToJSON req
        liftIO $ mark "GitHub request"
        (dur, res) <- liftIO $ clocked $
            GH.executeRequestWithMgr mgr auth req >>= either throwM pure
        let dur' = timeSpecToSecondsD dur
        logTrace ("GitHub request took " <> textShow dur') dur
        return res
