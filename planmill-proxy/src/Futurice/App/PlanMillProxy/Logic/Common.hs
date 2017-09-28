{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Futurice.App.PlanMillProxy.Logic.Common (
    module Futurice.Postgres,
    module Futurice.App.PlanMillProxy.Logic.Common,
    ) where

import Data.Aeson.Compat          (FromJSON)
import Data.Binary.Get            (Get, runGetOrFail)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, SemanticVersion, Version,
       structuralInfo, structuralInfoSha1ByteStringDigest)
import Data.Constraint
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (CachePolicy (..), genCachedIO)
import GHC.TypeLits               (natVal)
import Prelude ()

import PlanMill.Types.Query (Query (..), queryDict, queryToRequest)
import PlanMill.Worker      (submitPlanMill)

import Futurice.App.PlanMillProxy.Types (Ctx (..))

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'6 hours'"

capacityAge :: String
capacityAge = "'12 hours'"

-- Timereports are updated by updating oldest ones, so no age guarantee
-- atm

-- | Get previous @03:30@ (in @Europe/Helsinki@ timezone) from the given
-- timestamp.
previousThreeThirty :: UTCTime -> UTCTime
previousThreeThirty x
    | y < x     = y
    | otherwise = z
  where
    LocalTime d _ = utcToHelsinkiTime x
    tod = TimeOfDay 3 30 0
    y = helsinkiTimeToUtc (LocalTime d tod)
    z = helsinkiTimeToUtc (LocalTime (pred d) tod)

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
    :: forall a. (HasStructuralInfo a, HasSemanticVersion a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        ver <- get
        hash' <- get
        pure $ ver == ver' && hash' == hash''

    proxyV = Proxy :: Proxy (SemanticVersion a)
    proxyA = Proxy :: Proxy a
    ver' = fromIntegral (natVal proxyV) :: Version
    hash'' = structuralInfoSha1ByteStringDigest . structuralInfo $ proxyA
