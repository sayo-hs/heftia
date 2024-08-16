-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Concurrent.Timer where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Effect (sendIns, type (~>))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec)
import Data.Effect.Concurrent.Timer (LTimer, Timer (..), clock)
import Data.Function ((&))
import Data.Hefty.Extensible (ForallHFunctor, type (<|))
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO (liftIO)

runTimerIO ::
    forall eh ef.
    (IO <| ef, ForallHFunctor eh) =>
    eh :!! LTimer ': ef ~> eh :!! ef
runTimerIO =
    interpretRec \case
        Clock -> do
            t <- getMonotonicTimeNSec & liftIO
            pure $ picosecondsToDiffTime $ fromIntegral t * 1000
        Sleep t ->
            Thread.delay (diffTimeToPicoseconds t `quot` 1000_000) & liftIO

restartClock :: (Timer <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
restartClock a = do
    t0 <- clock
    a & interposeRec \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> sendIns other
