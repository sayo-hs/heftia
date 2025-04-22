{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo contributors

-- Benchmarking yield-intensive code

module BenchPyth where

import Control.Algebra qualified as F
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Carrier.NonDet.Church qualified as F
import Control.Carrier.Reader qualified as F
import Control.Monad (MonadPlus)
#ifdef VERSION_freer_simple
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.NonDet qualified as FS
import Control.Monad.Freer.Reader qualified as FS
#endif
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.NonDet qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Hefty.Shift qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Logic qualified as M
import Control.Monad.Reader qualified as M
import Data.List (singleton)
#ifdef VERSION_eff
import "eff" Control.Effect qualified as EF
#endif

#ifdef VERSION_freer_simple
programFreer :: (FS.Member FS.NonDet es) => Int -> FS.Eff es (Int, Int, Int)
programFreer upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n
{-# NOINLINE programFreer #-}

pythFreer :: Int -> [(Int, Int, Int)]
pythFreer n = FS.run $ FS.makeChoiceA $ programFreer n

pythFreerDeep :: Int -> [(Int, Int, Int)]
pythFreerDeep n = FS.run $ run $ run $ run $ run $ run $ FS.makeChoiceA $ run $ run $ run $ run $ run $ programFreer n
  where
    run = FS.runReader ()
#endif

programHeftia :: (H.Choose H.:> es, H.Empty H.:> es) => Int -> H.Eff es (Int, Int, Int)
programHeftia upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else H.empty
  where
    choice :: (H.Choose H.:> es, H.Empty H.:> es) => Int -> H.Eff es Int
    choice 0 = H.empty
    choice n = choice (n - 1) `H.branch` pure n
{-# NOINLINE programHeftia #-}

pythHeftia :: Int -> [(Int, Int, Int)]
pythHeftia n = H.runPure $ H.runNonDet $ programHeftia n

pythHeftiaDeep :: Int -> [(Int, Int, Int)]
pythHeftiaDeep n = H.runPure $ run $ run $ run $ run $ run $ H.runNonDet $ run $ run $ run $ run $ run $ programHeftia n
  where
    run :: H.Eff (H.Ask () ': es) a -> H.Eff es a
    run = H.runAsk ()

pythHeftiaShift :: Int -> [(Int, Int, Int)]
pythHeftiaShift n = H.runPure $ H.evalShift $ H.runNonDetShift $ singleton <$> programHeftia n

pythHeftiaShiftDeep :: Int -> [(Int, Int, Int)]
pythHeftiaShiftDeep n = H.runPure $ H.evalShift $ run $ run $ run $ run $ run $ H.runNonDetShift $ run $ run $ run $ run $ run $ singleton <$> programHeftia n
  where
    run :: H.Eff (H.Ask () ': es) a -> H.Eff es a
    run = H.runAsk ()

programFused :: (Monad m, Alternative m) => Int -> m (Int, Int, Int)
programFused upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice x = F.oneOf [1 .. x]
{-# NOINLINE programFused #-}

pythFused :: Int -> [(Int, Int, Int)]
pythFused n = F.run $ F.runNonDetA $ programFused n

pythFusedDeep :: Int -> [(Int, Int, Int)]
pythFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runNonDetA $ run $ run $ run $ run $ run $ programFused n
  where
    run = F.runReader ()

#ifdef VERSION_eff
programEff :: (EF.NonDet EF.:< es) => Int -> EF.Eff es (Int, Int, Int)
programEff upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n
{-# NOINLINE programEff #-}

pythEff :: Int -> [(Int, Int, Int)]
pythEff n = EF.run $ EF.runNonDetAll $ programEff n

pythEffDeep :: Int -> [(Int, Int, Int)]
pythEffDeep n = EF.run $ run $ run $ run $ run $ run $ EF.runNonDetAll $ run $ run $ run $ run $ run $ programEff n
  where
    run = EF.runReader ()
#endif

programMtl :: (MonadPlus m) => Int -> m (Int, Int, Int)
programMtl upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n
{-# NOINLINE programMtl #-}

pythLogict :: Int -> [(Int, Int, Int)]
pythLogict n = M.observeAll $ programMtl n

pythLogictDeep :: Int -> [(Int, Int, Int)]
pythLogictDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.observeAllT $ runR $ runR $ runR $ runR $ runR $ programMtl n
  where
    runR = (`M.runReaderT` ())
