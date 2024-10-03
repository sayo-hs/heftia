-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

-- Benchmarking effect invocation and monadic bind
module BenchCountdown where

import Control.Carrier.Reader qualified as F
import Control.Carrier.State.Strict qualified as F
import Control.Effect.Interpreter.Heftia.Reader qualified as H
import Control.Effect.Interpreter.Heftia.State qualified as H
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Freer.State qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State.Strict qualified as M
import Data.Effect.State qualified as H
import Effectful qualified as EL
import Effectful.Reader.Dynamic qualified as EL
import Effectful.State.Dynamic qualified as EL
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Polysemy.State qualified as P

programHeftia :: (H.Member (H.State Int) es) => H.Eff '[] es Int
programHeftia = do
    x <- H.get @Int
    if x == 0
        then pure x
        else do
            H.put (x - 1)
            programHeftia
{-# NOINLINE programHeftia #-}

countdownHeftia :: Int -> (Int, Int)
countdownHeftia n = H.runPure $ H.runState n programHeftia

countdownHeftiaDeep :: Int -> (Int, Int)
countdownHeftiaDeep n = H.runPure $ runR $ runR $ runR $ runR $ runR $ H.runState n $ runR $ runR $ runR $ runR $ runR $ programHeftia
  where
    runR = H.runAsk ()

programFreer :: (FS.Member (FS.State Int) es) => FS.Eff es Int
programFreer = do
    x <- FS.get @Int
    if x == 0
        then pure x
        else do
            FS.put (x - 1)
            programFreer
{-# NOINLINE programFreer #-}

countdownFreer :: Int -> (Int, Int)
countdownFreer n = FS.run $ FS.runState n programFreer

countdownFreerDeep :: Int -> (Int, Int)
countdownFreerDeep n = FS.run $ runR $ runR $ runR $ runR $ runR $ FS.runState n $ runR $ runR $ runR $ runR $ runR $ programFreer
  where
    runR = FS.runReader ()

programSem :: (P.Member (P.State Int) es) => P.Sem es Int
programSem = do
    x <- P.get @Int
    if x == 0
        then pure x
        else do
            P.put (x - 1)
            programSem
{-# NOINLINE programSem #-}

countdownSem :: Int -> (Int, Int)
countdownSem n = P.run $ P.runState n programSem

countdownSemDeep :: Int -> (Int, Int)
countdownSemDeep n = P.run $ runR $ runR $ runR $ runR $ runR $ P.runState n $ runR $ runR $ runR $ runR $ runR $ programSem
  where
    runR = P.runReader ()

programFused :: (F.Has (F.State Int) sig m) => m Int
programFused = do
    x <- F.get @Int
    if x == 0
        then pure x
        else do
            F.put (x - 1)
            programFused
{-# NOINLINE programFused #-}

countdownFused :: Int -> (Int, Int)
countdownFused n = F.run $ F.runState n programFused

countdownFusedDeep :: Int -> (Int, Int)
countdownFusedDeep n = F.run $ runR $ runR $ runR $ runR $ runR $ F.runState n $ runR $ runR $ runR $ runR $ runR $ programFused
  where
    runR = F.runReader ()

programEffectful :: (EL.State Int EL.:> es) => EL.Eff es Int
programEffectful = do
    x <- EL.get @Int
    if x == 0
        then pure x
        else do
            EL.put (x - 1)
            programEffectful
{-# NOINLINE programEffectful #-}

countdownEffectful :: Int -> (Int, Int)
countdownEffectful n = EL.runPureEff $ EL.runStateLocal n programEffectful

countdownEffectfulDeep :: Int -> (Int, Int)
countdownEffectfulDeep n =
    EL.runPureEff $ runR $ runR $ runR $ runR $ runR $ EL.runStateLocal n $ runR $ runR $ runR $ runR $ runR $ programEffectful
  where
    runR = EL.runReader ()

programMtl :: (M.MonadState Int m) => m Int
programMtl = do
    x <- M.get @Int
    if x == 0
        then pure x
        else do
            M.put (x - 1)
            programMtl
{-# NOINLINE programMtl #-}

countdownMtl :: Int -> (Int, Int)
countdownMtl = M.runState programMtl

countdownMtlDeep :: Int -> (Int, Int)
countdownMtlDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.runStateT (runR $ runR $ runR $ runR $ runR $ programMtl) n
  where
    runR = (`M.runReaderT` ())
