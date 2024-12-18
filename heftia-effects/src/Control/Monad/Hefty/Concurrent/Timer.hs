{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Timer]("Data.Effect.Concurrent.Timer") effects.
-}
module Control.Monad.Hefty.Concurrent.Timer (
    module Control.Monad.Hefty.Concurrent.Timer,
    module Data.Effect.Concurrent.Timer,
)
where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Monad.Hefty (
    interpose,
    interpret,
    liftIO,
    raise,
    raiseUnder,
    send,
    (&),
    type (:!!),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Coroutine (runCoroutine)
import Control.Monad.Hefty.State (evalState)
import Data.Effect.Concurrent.Timer
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.State (get, put)
import Data.Time (DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Void (Void, absurd)
import GHC.Clock (getMonotonicTimeNSec)

runTimerIO
    :: forall eh ef
     . (IO <| ef)
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

-- | Re-zeros the clock time in the local scope.
restartClock :: (Timer <| ef) => eh :!! ef ~> eh :!! ef
restartClock a = do
    t0 <- clock
    a & interpose \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> send other
