-- SPDX-License-Identifier: MPL-2.0

module Control.Effect.Interpreter.Heftia.Concurrent.Timer where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Effect (sendIns, type (~>))
import Control.Effect.Interpreter.Heftia.Coroutine (runCoroutine)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Monad.Hefty (interposeRec, interpret, interpretRec, raiseN, raiseNUnder, (:!!), type (<|))
import Data.Effect.Concurrent.Timer (CyclicTimer (Wait), Timer (..), clock, cyclicTimer)
import Data.Effect.Coroutine (Status (Coroutine, Done))
import Data.Effect.State (get, put)
import Data.Function ((&))
import Data.Time (DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Void (Void, absurd)
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO (liftIO)

runTimerIO
    :: forall eh ef
     . (IO <| ef)
    => eh :!! Timer ': ef ~> eh :!! ef
runTimerIO =
    interpretRec \case
        Clock -> do
            t <- getMonotonicTimeNSec & liftIO
            pure $ picosecondsToDiffTime $ fromIntegral t * 1000
        Sleep t ->
            Thread.delay (diffTimeToPicoseconds t `quot` 1000_000) & liftIO

runCyclicTimer :: forall ef. (Timer <| ef) => '[] :!! CyclicTimer ': ef ~> '[] :!! ef
runCyclicTimer a = do
    timer0 :: Status ('[] :!! ef) () DiffTime Void <- runCoroutine cyclicTimer
    a
        & raiseNUnder @1 @1
        & interpret \case
            Wait delta ->
                get @(Status ('[] :!! ef) () DiffTime Void) >>= \case
                    Done x -> absurd x
                    Coroutine () k -> put =<< raiseN @1 (k delta)
        & evalState timer0

restartClock :: (Timer <| ef) => eh :!! ef ~> eh :!! ef
restartClock a = do
    t0 <- clock
    a & interposeRec \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> sendIns other
