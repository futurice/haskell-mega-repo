{-# LANGUAGE GADTs #-}
module Control.Monad.Http.Operational where

import Prelude        ()
import Prelude.Compat

import Control.Monad.Operational

import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client  as H

-- | Instructions needed for implement 'MonadHttp'
data HttpInstr a where
    HttpLbs :: H.Request -> HttpInstr (H.Response L.ByteString)

type HttpOpT = ProgramT HttpInstr

-- | @HttpOpT Identity@
type HttpOp = Program HttpInstr

runHttpOpT :: Monad m
           => (H.Request -> m (H.Response L.ByteString))
           -> HttpOpT m a -> m a
runHttpOpT hlbs = go
  where
    go m = do
        v <- viewT m
        case v of
            Return a           -> return a
            HttpLbs req :>>= k -> do
                b <- hlbs req
                go (k b)

runHttpOp :: Monad m
          => (H.Request -> m (H.Response L.ByteString))
          -> HttpOp a -> m a
runHttpOp hlbs = go
  where
    go m = case view m of
        Return a           -> return a
        HttpLbs req :>>= k -> do
            b <- hlbs req
            go (k b)
