{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Parallel]("Data.Effect.Concurrent.Parallel") effects.
-}
module Control.Monad.Hefty.Concurrent.Parallel (
    module Control.Monad.Hefty.Concurrent.Parallel,
    module Data.Effect.Concurrent.Parallel,
    module Data.Effect.Input,
)
where

import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    interpret,
    interpretH,
    raiseAllH,
    transform,
    type (<<|),
    type (<|),
    type (~>),
    type (~~>),
 )
import Control.Monad.Hefty.Coroutine (inputToYield, runCoroutine)
import Control.Monad.Hefty.Unlift (UnliftIO)
import Data.Effect.Concurrent.Parallel
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.Input
import Data.Function (fix)
import UnliftIO (
    MonadIO,
    MonadUnliftIO,
    atomically,
    liftIO,
    mask,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryReadTMVar,
    uninterruptibleMask_,
    withRunInIO,
 )
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)

runConcurrentIO
    :: (UnliftIO <<| eh, IO <| ef)
    => Eff (Parallel ': Race ': Poll ': eh) (Halt ': ef) ~> Eff eh ef
runConcurrentIO = runHaltIO . runPollIO . runRaceIO . runParallelIO

runParallelIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Parallel ': eh) ef ~> Eff eh ef
runParallelIO = interpretH parallelToIO

parallelToIO :: (MonadUnliftIO m) => Parallel ~~> m
parallelToIO (LiftP2 f a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            t <- forkIO do
                x <- restore $ run a
                atomically $ putTMVar var x

            y <- restore $ run b

            atomically do
                x <- readTMVar var
                pure $ f x y
                <* uninterruptibleMask_ (killThread t)
{-# INLINE parallelToIO #-}

runPollIO :: (IO <| ef, UnliftIO <<| eh) => Eff (Poll ': eh) ef ~> Eff eh ef
runPollIO = interpretH pollToIO

runRaceIO :: (IO <| ef, UnliftIO <<| eh) => Eff (Race ': eh) ef ~> Eff eh ef
runRaceIO = interpretH raceToIO

runHaltIO :: (IO <| ef) => Eff eh (Halt ': ef) ~> Eff eh ef
runHaltIO = interpret haltToIO

raceToIO :: (MonadUnliftIO m) => Race ~~> m
raceToIO (Race a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            let runThread m = forkIO do
                    x <- restore $ run m
                    atomically $ putTMVar var x

            t1 <- runThread a
            t2 <- runThread b

            atomically (readTMVar var)
                <* uninterruptibleMask_ (killThread t1 *> killThread t2)

pollToIO :: (MonadUnliftIO m) => Poll ~~> m
pollToIO (Poldl f a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            t <- forkIO do
                x <- restore $ run b
                atomically $ putTMVar var x

            restore (run a) >>= fix \next acc -> do
                poll <- atomically $ tryReadTMVar var
                restore (run $ f acc poll) >>= \case
                    Left r -> do
                        uninterruptibleMask_ $ killThread t
                        pure r
                    Right acc' -> next acc'

haltToIO :: (MonadIO m) => Halt ~> m
haltToIO Halt = liftIO $ forever $ threadDelay maxBound

runParallelAsSequential :: Eff (Parallel ': eh) ef ~> Eff eh ef
runParallelAsSequential = interpretH parallelToSequential

parallelToSequential :: Parallel ~~> Eff eh ef
parallelToSequential (LiftP2 f a b) = liftA2 f a b

polling :: (Poll <<| eh) => Eff eh ef a -> Eff '[] (Input (Maybe a) ': ef) r -> Eff eh ef r
polling pollee poller =
    poldl
        ( \case
            Done r -> const $ pure $ Left r
            Continue () k -> fmap Right . raiseAllH . k
        )
        (raiseAllH $ runCoroutine $ transform inputToYield poller)
        pollee

runForAsParallel :: (Parallel <<| eh, Traversable t) => Eff (For t ': eh) ef ~> Eff eh ef
runForAsParallel = interpretH forToParallel
