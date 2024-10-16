{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Parallel (
    module Control.Monad.Hefty.Concurrent.Parallel,
    module Data.Effect.Concurrent.Parallel,
)
where

import Control.Applicative ((<|>))
import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    interpret,
    interpretH,
    type (<<|),
    type (<|),
    type (~>),
    type (~~>),
 )
import Control.Monad.Hefty.Unlift (UnliftIO)
import Data.Effect.Concurrent.Parallel
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO qualified as IO
import UnliftIO.Concurrent (threadDelay)

runConcurrentIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Race ': Parallel ': eh) (Halt ': ef) ~> Eff eh ef
runConcurrentIO = runHaltIO . runParallelIO . runRaceIO

runParallelIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Parallel ': eh) ef ~> Eff eh ef
runParallelIO = interpretH parallelToIO

runRaceIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Race ': eh) ef ~> Eff eh ef
runRaceIO = interpretH raceToIO

runHaltIO :: (IO <| ef) => Eff eh (Halt ': ef) ~> Eff eh ef
runHaltIO = interpret haltToIO

parallelToIO :: (MonadUnliftIO m) => Parallel ~~> m
parallelToIO (LiftP2 f a b) = IO.runConcurrently $ liftA2 f (IO.Concurrently a) (IO.Concurrently b)
{-# INLINE parallelToIO #-}

raceToIO :: (MonadUnliftIO m) => Race ~~> m
raceToIO (Race a b) = IO.runConcurrently $ IO.Concurrently a <|> IO.Concurrently b
{-# INLINE raceToIO #-}

haltToIO :: (MonadIO m) => Halt ~> m
haltToIO Halt = liftIO $ forever $ threadDelay maxBound
{-# INLINE haltToIO #-}

runForAsParallel :: (Parallel <<| eh, Traversable t) => Eff (For t ': eh) ef ~> Eff eh ef
runForAsParallel = interpretH forToParallel
