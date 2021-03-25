{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Futurice.App.PlanMillProxy.Logic.Common (
    module Futurice.Postgres,
    module Futurice.App.PlanMillProxy.Logic.Common,
    ) where

import Data.Aeson.Compat  (FromJSON)
import Data.Binary.Get    (Get, runGetOrFail)
import Data.Binary.Tagged
       (Structure, Structured, TypeVersion, structure, structureHash,
       typeVersion)
import Data.Constraint
import Futurice.Postgres  hiding (Query)
import Futurice.Prelude
import Futurice.Servant   (CachePolicy (..), genCachedIO)
import GHC.TypeLits       (natVal)
import Prelude ()

import PlanMill.Types.Query (Query (..), queryDict, queryToRequest)
import PlanMill.Worker      (submitPlanMill)

import Futurice.App.PlanMillProxy.Types (Ctx (..))

-------------------------------------------------------------------------------
-- LIO
-------------------------------------------------------------------------------

type LIO = LogT IO

runLIO :: Ctx -> LIO a -> IO a
runLIO =  runLogT'

runLogT' :: Ctx -> LogT IO a -> IO a
runLogT' ctx = runLogT "logic" (ctxLogger ctx)

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromPlanMill :: Ctx -> Query a -> LIO a
fetchFromPlanMill ctx q = case (typeableDict, fromJsonDict, nfdataDict) of
    (Dict, Dict, Dict) -> liftIO $
        genCachedIO RequestNew logger cache (10 * 60) q $
            submitPlanMill ws (queryToRequest q)
  where
    typeableDict = queryDict (Proxy :: Proxy Typeable) q
    fromJsonDict = queryDict (Proxy :: Proxy FromJSON) q
    nfdataDict   = queryDict (Proxy :: Proxy NFData) q

    ws     = ctxWorkers ctx
    logger = ctxLogger ctx
    cache  = ctxCache ctx

-------------------------------------------------------------------------------
-- binary-tagged additions
-------------------------------------------------------------------------------

-- | Check whether the tag at the beginning of the 'LazyByteString' is correct.
checkTagged
    :: forall a. (Structured a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        hash' <- get
        pure $ hash' == hash''

    proxyA = Proxy :: Proxy a
    hash'' = structureHash proxyA
