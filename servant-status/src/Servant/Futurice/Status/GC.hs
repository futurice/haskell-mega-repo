{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Servant.Futurice.Status.GC (
    gcStatusInfo,
    ) where

import Servant.Futurice.Status.Types

#if MIN_VERSION_base(4,10,0)
gcStatusInfo :: StatusInfoIO
gcStatusInfo = SIIO $ pure mempty

#else

import Prelude ()
import Prelude.Compat
import GHC.Stats
import Data.Semigroup (sconcat)
import System.Mem     (performMinorGC)

gcStatsToStatusInfo :: GCStats -> StatusInfo
gcStatsToStatusInfo GCStats{..} = group "gc" $ sconcat
    [ metric "bytesAllocated" bytesAllocated
    , metric "numGcs" numGcs
    , metric "maxBytesUsed" maxBytesUsed
    , metric "numByteUsageSamples" numByteUsageSamples
    , metric "cumulativeBytesUsed" cumulativeBytesUsed
    , metric "bytesCopied" bytesCopied
    , metric "currentBytesUsed" currentBytesUsed
    , metric "currentBytesSlop" currentBytesSlop
    , metric "maxBytesSlop" maxBytesSlop
    , metric "peakMegabytesAllocated" peakMegabytesAllocated
    , group "par" $ sconcat
        [ metric "totBytesCopied" parTotBytesCopied
        , metric "maxBytesCopied" parMaxBytesCopied
        ]
    , group "wall" $ sconcat
        [ metric "mutatorSeconds" mutatorWallSeconds
        , metric "gcSeconds" gcWallSeconds
        , metric "seconds" wallSeconds
        ]
    , group "cpu" $ sconcat
        [ metric "mutatorSeconds" mutatorCpuSeconds
        , metric "gcSeconds" gcCpuSeconds
        , metric "seconds" cpuSeconds
        ]
    ]

gcStatusInfo :: StatusInfoIO
gcStatusInfo = SIIO $ gcStatsToStatusInfo <$> (performMinorGC >> getGCStats)
#endif
