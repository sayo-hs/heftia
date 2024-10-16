{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Parallel where

import Control.Applicative (Alternative, empty, liftA3, (<|>))
import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    Type,
    interpret,
    interpretH,
    makeEffect,
    makeEffectH_,
    type (<:),
    type (<<:),
    type (<<|),
    type (<|),
    type (~>),
    type (~~>),
 )
import Control.Monad.Hefty.Unlift (UnliftIO)
import Data.Effect.HFunctor.TH (makeHFunctor')
import Data.List.Infinite (Infinite ((:<)))
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO qualified as IO
import UnliftIO.Concurrent (threadDelay)

data Parallel f a where
    LiftP2 :: (a -> b -> c) -> f a -> f b -> Parallel f c

data Halt (a :: Type) where
    Halt :: Halt a

data Race f (a :: Type) where
    Race :: f a -> f a -> Race f a

makeEffect [''Halt] [''Parallel, ''Race]

newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (Parallel <<: f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    {-# INLINE pure #-}

    liftA2 f (Concurrently a) (Concurrently b) = Concurrently $ liftP2 f a b
    {-# INLINE liftA2 #-}

instance (Race <<: f, Halt <: f, Parallel <<: f, Applicative f) => Alternative (Concurrently f) where
    empty = Concurrently halt
    {-# INLINE empty #-}

    (Concurrently a) <|> (Concurrently b) = Concurrently $ race a b
    {-# INLINE (<|>) #-}

liftP3 :: (Parallel <<: f, Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftP3 f a b c = runConcurrently $ liftA3 f (Concurrently a) (Concurrently b) (Concurrently c)
{-# INLINE liftP3 #-}

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

data For (t :: Type -> Type) f a where
    For :: t (f a) -> For t f (t a)
makeEffectH_ [''For]
makeHFunctor' ''For \(t :< _) -> [t|Functor $t|]

forToParallel :: (Parallel <<| eh, Traversable t) => For t ~~> Eff eh ef
forToParallel (For iters) = runConcurrently $ traverse Concurrently iters
