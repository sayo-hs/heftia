-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

-- Benchmarking higher-order effects #2: Local environments

module BenchLocal where

import Control.Carrier.Reader qualified as F
import Control.Monad.Hefty.Types qualified as H
import Data.Effect.OpenUnion qualified as H
import Data.Effect.Reader qualified as H
import Effectful qualified as EL
import Effectful.Reader.Dynamic qualified as EL
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import "data-effects" Control.Effect.Interpret qualified as HD
import "eff" Control.Effect qualified as E

programHeftia :: (H.Ask Int `H.In` es, H.Local Int `H.In` es) => Int -> H.Eff es Int
programHeftia = \case
    0 -> H.ask'_
    n -> H.local'_ @Int (+ 1) (programHeftia (n - 1))
{-# NOINLINE programHeftia #-}

localHeftia :: Int -> Int
localHeftia n = HD.runPure $ H.runAsk @Int 0 $ H.runLocal @Int $ programHeftia n

localHeftiaDeep0, localHeftiaDeep1, localHeftiaDeep2, localHeftiaDeep3, localHeftiaDeep4, localHeftiaDeep5 :: Int -> Int
localHeftiaDeep0 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runLocal @Int $ programHeftia n
localHeftiaDeep1 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ hrun $ hrun $ hrun $ hrun $ H.runLocal @Int $ hrun $ programHeftia n
localHeftiaDeep2 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ hrun $ hrun $ hrun $ H.runLocal @Int $ hrun $ hrun $ programHeftia n
localHeftiaDeep3 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ hrun $ hrun $ H.runLocal @Int $ hrun $ hrun $ hrun $ programHeftia n
localHeftiaDeep4 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ hrun $ H.runLocal @Int $ hrun $ hrun $ hrun $ hrun $ programHeftia n
localHeftiaDeep5 n = HD.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runAsk @Int 0 $ H.runLocal @Int $ hrun $ hrun $ hrun $ hrun $ hrun $ programHeftia n

hrun :: H.Eff (H.Ask () ': es) a -> H.Eff es a
hrun = H.runAsk ()

programSem :: (P.Reader Int `P.Member` es) => Int -> P.Sem es Int
programSem = \case
    0 -> P.ask
    n -> P.local @Int (+ 1) (programSem (n - 1))
{-# NOINLINE programSem #-}

localSem :: Int -> Int
localSem n = P.run $ P.runReader @Int 0 $ programSem n

localSemDeep :: Int -> Int
localSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runReader @Int 0 $ run $ run $ run $ run $ run $ programSem n
  where
    run = P.runReader ()

programFused :: (F.Has (F.Reader Int) sig m) => Int -> m Int
programFused = \case
    0 -> F.ask
    n -> F.local @Int (+ 1) (programFused (n - 1))
{-# NOINLINE programFused #-}

localFused :: Int -> Int
localFused n = F.run $ F.runReader @Int 0 $ programFused n

localFusedDeep :: Int -> Int
localFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runReader @Int 0 $ run $ run $ run $ run $ run $ programFused n
  where
    run = F.runReader ()

programEffectful :: (EL.Reader Int EL.:> es) => Int -> EL.Eff es Int
programEffectful = \case
    0 -> EL.ask
    n -> EL.local @Int (+ 1) (programEffectful (n - 1))
{-# NOINLINE programEffectful #-}

localEffectful :: Int -> Int
localEffectful n = EL.runPureEff $ EL.runReader @Int 0 $ programEffectful n

localEffectfulDeep :: Int -> Int
localEffectfulDeep n =
    EL.runPureEff $ run $ run $ run $ run $ run $ EL.runReader @Int 0 $ run $ run $ run $ run $ run $ programEffectful n
  where
    run = EL.runReader ()

programEff :: (E.Reader Int E.:< es) => Int -> E.Eff es Int
programEff = \case
    0 -> E.ask
    n -> E.local @Int (+ 1) (programEff (n - 1))
{-# NOINLINE programEff #-}

localEff :: Int -> Int
localEff n = E.run $ E.runReader @Int 0 $ programEff n

localEffDeep :: Int -> Int
localEffDeep n = E.run $ run $ run $ run $ run $ run $ E.runReader @Int 0 $ run $ run $ run $ run $ run $ programEff n
  where
    run = E.runReader ()

{-
The MTL case is disabled because of conflicting functional dependencies.

When using something other than Reader to build up the stack, it is considered
that the performance degradation caused by the pure stack factor cannot be
measured.

programMtl :: (M.MonadReader Int m) => Int -> m Int
programMtl = \case
    0 -> M.ask
    n -> M.local (+ 1) (programMtl (n - 1))
{-# NOINLINE programMtl #-}

localMtl :: Int -> Int
localMtl n = M.runReader 0 $ programMtl n

localMtlDeep :: Int -> Int
localMtlDeep n = M.runIdentity $ run $ run $ run $ run $ run $ M.runReader 0 $ run $ run $ run $ run $ run $ programMtl n
  where
    run = (`M.runReaderT` ())
-}
