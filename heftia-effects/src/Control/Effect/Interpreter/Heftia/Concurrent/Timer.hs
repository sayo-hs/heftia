-- SPDX-License-Identifier: MPL-2.0

module Control.Effect.Interpreter.Heftia.Concurrent.Timer where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Effect (type (~>))
import Control.Effect.Interpreter.Heftia.Coroutine (runCoroutine)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Monad.Hefty.Interpret (interpose, interpret)
import Control.Monad.Hefty.Transform (raise, raiseUnder)
import Control.Monad.Hefty.Types (send, (:!!))
import Data.Effect.Concurrent.Timer (CyclicTimer (Wait), Timer (..), clock, cyclicTimer)
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.OpenUnion.Internal.HO (HFunctors)
import Data.Effect.State (get, put)
import Data.Function ((&))
import Data.Time (DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Void (Void, absurd)
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO (liftIO)

runTimerIO
    :: forall eh ef
     . (IO <| ef, HFunctors eh)
    => eh :!! Timer ': ef ~> eh :!! ef
runTimerIO =
    interpret \case
        Clock -> do
            t <- getMonotonicTimeNSec & liftIO
            pure $ picosecondsToDiffTime $ fromIntegral t * 1000
        Sleep t ->
            Thread.delay (diffTimeToPicoseconds t `quot` 1000_000) & liftIO

runCyclicTimer
    :: forall ef
     . (Timer <| ef)
    => '[] :!! CyclicTimer ': ef ~> '[] :!! ef
runCyclicTimer a = do
    timer0 :: Status ('[] :!! ef) () DiffTime Void <- runCoroutine cyclicTimer
    a
        & raiseUnder
        & interpret \case
            Wait delta ->
                get @(Status ('[] :!! ef) () DiffTime Void) >>= \case
                    Done x -> absurd x
                    Continue () k -> put =<< raise (k delta)
        & evalState timer0

restartClock :: (Timer <| ef, HFunctors eh) => eh :!! ef ~> eh :!! ef
restartClock a = do
    t0 <- clock
    a & interpose \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> send other
