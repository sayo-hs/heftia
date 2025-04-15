-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo contributors

-- Benchmarking yield-intensive code
module BenchPyth where

import Control.Algebra qualified as F
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Carrier.NonDet.Church qualified as F
import Control.Carrier.Reader qualified as F
import Control.Ev.Eff qualified as E
import Control.Ev.Util qualified as E
import Control.Monad (MonadPlus)
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.NonDet qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.NonDet qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Logic qualified as M
import Control.Monad.Reader qualified as M
import Control.Mp.Eff qualified as Mp
import Control.Mp.Util qualified as Mp
import "eff" Control.Effect qualified as EF

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

programEv :: (E.Choose E.:? e) => Int -> E.Eff e (Int, Int, Int)
programEv upbound = do
    x <- E.perform E.choose upbound
    y <- E.perform E.choose upbound
    z <- E.perform E.choose upbound
    if x * x + y * y == z * z then return (x, y, z) else E.perform (\r -> E.none r) ()
{-# NOINLINE programEv #-}

pythEv :: Int -> [(Int, Int, Int)]
pythEv n = E.runEff $ E.chooseAll $ programEv n

pythEvDeep :: Int -> [(Int, Int, Int)]
pythEvDeep n = E.runEff $ run $ run $ run $ run $ run $ E.chooseAll $ run $ run $ run $ run $ run $ programEv n
  where
    run = E.reader ()

programMp :: (Mp.Choose Mp.:? e) => Int -> Mp.Eff e (Int, Int, Int)
programMp upbound = do
    x <- Mp.perform Mp.choose upbound
    y <- Mp.perform Mp.choose upbound
    z <- Mp.perform Mp.choose upbound
    if x * x + y * y == z * z then return (x, y, z) else Mp.perform (\r -> Mp.none r) ()
{-# NOINLINE programMp #-}

pythMp :: Int -> [(Int, Int, Int)]
pythMp n = Mp.runEff $ Mp.chooseAll $ programMp n

pythMpDeep :: Int -> [(Int, Int, Int)]
pythMpDeep n = Mp.runEff $ run $ run $ run $ run $ run $ Mp.chooseAll $ run $ run $ run $ run $ run $ programMp n
  where
    run = Mp.reader ()

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
