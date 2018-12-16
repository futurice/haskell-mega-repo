{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'MonadHttp' class with basic HTTP functionality.
----------------------------------------------------------------------------
module Control.Monad.Http (
    -- * Class
    MonadHttp(..),
    MonadStreamingHttp(..),
    BodyReaderM,
    -- * Transformer
    HttpT(..),
    evalHttpT,
    -- * Utilities
    defaultHttpLbs,
    brConsume,
    -- * Re-exports
    Request(..),
    Response(..),
    ) where

import Control.Monad.Http.Class
import Control.Monad.Trans.Http
import Network.HTTP.Client      (Request (..), Response (..))
