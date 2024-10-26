{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Parallel (
    module Control.Monad.Hefty.Concurrent.Parallel,
    module Data.Effect.Concurrent.Parallel,
    module Data.Effect.NonDet,
    module Data.Effect.Input,
)
where

import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    Interpreter,
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
import Data.Effect.NonDet
import Data.Function (fix)
import GHC.Conc (retry)
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
import UnliftIO qualified as IO
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)

runConcurrentIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Parallel ': Race ': Poll ': eh) (Halt ': ef) ~> Eff eh ef
runConcurrentIO = runHaltIO . runPollIO . runRaceIO . runParallelIO

runParallelIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Parallel ': eh) ef ~> Eff eh ef
runParallelIO = interpretH parallelToIO

parallelToIO :: (MonadUnliftIO m) => Parallel ~~> m
parallelToIO (LiftP2 f a b) = IO.runConcurrently $ liftA2 f (IO.Concurrently a) (IO.Concurrently b)
{-# INLINE parallelToIO #-}

runPollIO :: (IO <| ef, UnliftIO <<| eh) => Eff (Poll ': eh) ef ~> Eff eh ef
runPollIO = interpretH elabPollIO

runRaceIO :: (IO <| ef, UnliftIO <<| eh) => Eff (Race ': eh) ef ~> Eff eh ef
runRaceIO = interpretH elabRaceIO

runHaltIO :: (IO <| ef) => Eff eh (Halt ': ef) ~> Eff eh ef
runHaltIO = interpret handleHaltIO

elabRaceIO :: (MonadUnliftIO m) => Race ~~> m
elabRaceIO (Race a b) =
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

elabPollIO :: (MonadUnliftIO m) => Poll ~~> m
elabPollIO (Poldl f a b) =
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

handleHaltIO :: (MonadIO m) => Halt ~> m
handleHaltIO Halt = liftIO $ forever $ threadDelay maxBound

handleChooseRaceIO :: (MonadUnliftIO m) => Interpreter Choose m ans
handleChooseRaceIO Choose k = elabRaceIO $ Race (k False) (k True)

handleEmptyHaltIO :: (MonadIO m) => Interpreter Empty m ans
handleEmptyHaltIO Empty _ = liftIO $ atomically retry

runSequential :: Eff (Parallel ': eh) ef ~> Eff eh ef
runSequential = interpretH sequential

sequential :: Parallel ~~> Eff eh ef
sequential (LiftP2 f a b) = liftA2 f a b

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
