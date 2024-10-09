-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

module BenchCoroutine where

import Control.Monad (forM)
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Coroutine qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Coroutine qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Mp.Eff qualified as Mp
import Control.Mp.Util qualified as Mp
import Data.Effect.Coroutine qualified as H
import "eff" Control.Effect qualified as E

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

programHeftia :: (H.Member (H.Yield Int Int) es) => Int -> H.Eff '[] es [Int]
programHeftia upbound =
    forM [1 .. upbound] H.yield
{-# NOINLINE programHeftia #-}

loopStatusHeftia :: H.Status (H.Eff '[] ef) Int Int r -> H.Eff '[] ef r
loopStatusHeftia = \case
    H.Done r -> pure r
    H.Continue i f -> loopStatusHeftia =<< f (i + 100)
{-# NOINLINE loopStatusHeftia #-}

coroutineHeftia :: Int -> [Int]
coroutineHeftia n = H.runPure $ loopStatusHeftia =<< H.runCoroutine (programHeftia n)

coroutineHeftiaDeep :: Int -> [Int]
coroutineHeftiaDeep n = H.runPure $ run $ run $ run $ run $ run $ loopStatusHeftia =<< H.runCoroutine (run $ run $ run $ run $ run $ programHeftia n)
  where
    run = H.runAsk ()

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

programMp :: (MpYield Int Int Mp.:? e) => Int -> Mp.Eff e [Int]
programMp n = forM [0 .. n] $ \i -> Mp.perform mpYield i
{-# NOINLINE programMp #-}

loopStatusMp :: H.Status (Mp.Eff e) Int Int r -> Mp.Eff e r
loopStatusMp = \case
    H.Done r -> pure r
    H.Continue a k -> loopStatusMp =<< k (a + 100)
{-# NOINLINE loopStatusMp #-}

coroutineMp :: Int -> [Int]
coroutineMp n = Mp.runEff $ loopStatusMp =<< mpCoroutine @Int @Int (programMp n)

coroutineMpDeep :: Int -> [Int]
coroutineMpDeep n = Mp.runEff $ run $ run $ run $ run $ run $ loopStatusMp =<< mpCoroutine @Int @Int (run $ run $ run $ run $ run $ programMp n)
  where
    run = Mp.reader ()

newtype MpYield a b e ans = MpYield {mpYield :: Mp.Op a b e ans}

mpCoroutine :: Mp.Eff (MpYield a b Mp.:* e) r -> Mp.Eff e (H.Status (Mp.Eff e) a b r)
mpCoroutine = Mp.handler MpYield {mpYield = Mp.operation $ \a k -> pure $ H.Continue a k} . fmap H.Done
