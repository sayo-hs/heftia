{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo contributors

module BenchCoroutine where

import Control.Monad (forM)
#ifdef VERSION_freer_simple
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Coroutine qualified as FS
import Control.Monad.Freer.Reader qualified as FS
#endif
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Coroutine qualified as H
import Control.Monad.Hefty.Reader qualified as H
#ifdef VERSION_eff
import "eff" Control.Effect qualified as E
#endif

#ifdef VERSION_freer_simple
programFreer :: (FS.Member (FS.Yield Int Int) es) => Int -> FS.Eff es [Int]
programFreer upbound =
    forM [1 .. upbound] (`FS.yield` id)
{-# NOINLINE programFreer #-}

loopStatusFreer :: FS.Status es Int Int r -> FS.Eff es r
loopStatusFreer = \case
    FS.Done r -> pure r
    FS.Continue i f -> loopStatusFreer =<< f (i + 100)
{-# NOINLINE loopStatusFreer #-}

coroutineFreer :: Int -> [Int]
coroutineFreer n = FS.run $ loopStatusFreer =<< FS.runC (programFreer n)

coroutineFreerDeep :: Int -> [Int]
coroutineFreerDeep n = FS.run $ run $ run $ run $ run $ run $ loopStatusFreer =<< FS.runC (run $ run $ run $ run $ run $ programFreer n)
  where
    run = FS.runReader ()
#endif

programHeftia :: (H.Yield Int Int H.:> es) => Int -> H.Eff es [Int]
programHeftia upbound =
    forM [1 .. upbound] H.yield
{-# NOINLINE programHeftia #-}

loopStatusHeftia :: H.Status (H.Eff es) Int Int r -> H.Eff es r
loopStatusHeftia = \case
    H.Done r -> pure r
    H.Continue i f -> loopStatusHeftia =<< f (i + 100)
{-# NOINLINE loopStatusHeftia #-}

coroutineHeftia :: Int -> [Int]
coroutineHeftia n = H.runPure $ loopStatusHeftia =<< H.runCoroutine (programHeftia n)

coroutineHeftiaDeep :: Int -> [Int]
coroutineHeftiaDeep n = H.runPure $ run $ run $ run $ run $ run $ loopStatusHeftia =<< H.runCoroutine (run $ run $ run $ run $ run $ programHeftia n)
  where
    run :: H.Eff (H.Ask () ': es) a -> H.Eff es a
    run = H.runAsk ()

#ifdef VERSION_eff
programEff :: (E.Coroutine Int Int E.:< es) => Int -> E.Eff es [Int]
programEff upbound =
    forM [1 .. upbound] $ E.yield @Int @Int
{-# NOINLINE programEff #-}

loopStatusEff :: E.Status es Int Int r -> E.Eff es r
loopStatusEff = \case
    E.Done r -> pure r
    E.Yielded i f -> loopStatusEff =<< E.runCoroutine (f (i + 100))
{-# NOINLINE loopStatusEff #-}

coroutineEff :: Int -> [Int]
coroutineEff n = E.run $ loopStatusEff =<< E.runCoroutine (programEff n)

coroutineEffDeep :: Int -> [Int]
coroutineEffDeep n = E.run $ run $ run $ run $ run $ run $ loopStatusEff =<< E.runCoroutine (run $ run $ run $ run $ run $ programEff n)
  where
    run = E.runReader ()
#endif
