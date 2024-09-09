-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.Concurrent.Timer where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Effect (sendIns, type (~>))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interposeRec, interpret, interpretRec, raise, raiseUnder)
import Control.Effect.Interpreter.Heftia.Coroutine (runCoroutine)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Data.Effect.Concurrent.Timer (CyclicTimer (Wait), LCyclicTimer, LTimer, Timer (..), clock, cyclicTimer)
import Data.Effect.Coroutine (Status (Coroutine, Done))
import Data.Effect.State (get, put)
import Data.Function ((&))
import Data.Hefty.OpenUnion (ForallHFunctor, type (<|))
import Data.Time (DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Void (Void, absurd)
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

runCyclicTimer :: forall ef. Timer <| ef => '[] :!! LCyclicTimer ': ef ~> '[] :!! ef
runCyclicTimer a = do
    timer0 :: Status ('[] :!! ef) () DiffTime Void <- runCoroutine cyclicTimer
    a & raiseUnder
        & interpret \case
            Wait delta ->
                get @(Status ('[] :!! ef) () DiffTime Void) >>= \case
                    Done x -> absurd x
                    Coroutine () k -> put =<< raise (k delta)
        & evalState timer0

restartClock :: (Timer <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
restartClock a = do
    t0 <- clock
    a & interposeRec \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> sendIns other
