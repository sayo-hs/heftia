-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

-- Benchmarking scoped effects #1: Catching errors

module BenchCatch where

import Control.Carrier.Error.Either qualified as F
import Control.Carrier.Reader qualified as F
import Control.Monad.Except qualified as M
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Except qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Identity qualified as M
import Control.Monad.Reader qualified as M
import Effectful qualified as EL
import Effectful.Error.Dynamic qualified as EL
import Effectful.Reader.Dynamic qualified as EL
import Polysemy qualified as P
import Polysemy.Error qualified as P
import Polysemy.Reader qualified as P
import "eff" Control.Effect qualified as E

programHeftia :: (H.Member (H.Throw ()) ef, H.MemberH (H.Catch ()) eh) => Int -> H.Eff eh ef a
programHeftia = \case
    0 -> H.throw ()
    n -> H.catch (programHeftia (n - 1)) \() -> H.throw ()
{-# NOINLINE programHeftia #-}

catchHeftia :: Int -> Either () ()
catchHeftia n = H.runPure $ H.runThrow $ H.runCatch @() $ programHeftia n

catchHeftiaDeep0, catchHeftiaDeep1, catchHeftiaDeep2, catchHeftiaDeep3, catchHeftiaDeep4, catchHeftiaDeep5 :: Int -> Either () ()
catchHeftiaDeep0 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runCatch @() $ programHeftia n
catchHeftiaDeep1 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ hrun $ hrun $ hrun $ hrun $ H.runCatch @() $ hrun $ programHeftia n
catchHeftiaDeep2 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ hrun $ hrun $ hrun $ H.runCatch @() $ hrun $ hrun $ programHeftia n
catchHeftiaDeep3 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ hrun $ hrun $ H.runCatch @() $ hrun $ hrun $ hrun $ programHeftia n
catchHeftiaDeep4 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ hrun $ H.runCatch @() $ hrun $ hrun $ hrun $ hrun $ programHeftia n
catchHeftiaDeep5 n = H.runPure $ hrun $ hrun $ hrun $ hrun $ hrun $ H.runThrow $ H.runCatch @() $ hrun $ hrun $ hrun $ hrun $ hrun $ programHeftia n

hrun :: H.Eff eh (H.Ask () ': ef) a -> H.Eff eh ef a
hrun = H.runAsk ()

programSem :: (P.Error () `P.Member` es) => Int -> P.Sem es a
programSem = \case
    0 -> P.throw ()
    n -> P.catch (programSem (n - 1)) \() -> P.throw ()
{-# NOINLINE programSem #-}

catchSem :: Int -> Either () ()
catchSem n = P.run $ P.runError $ programSem n

catchSemDeep :: Int -> Either () ()
catchSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runError $ run $ run $ run $ run $ run $ programSem n
  where
    run = P.runReader ()

programFused :: (F.Has (F.Error ()) sig m) => Int -> m a
programFused = \case
    0 -> F.throwError ()
    n -> F.catchError (programFused (n - 1)) \() -> F.throwError ()
{-# NOINLINE programFused #-}

catchFused :: Int -> Either () ()
catchFused n = F.run $ F.runError $ programFused n

catchFusedDeep :: Int -> Either () ()
catchFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runError $ run $ run $ run $ run $ run $ programFused n
  where
    run = F.runReader ()

programEffectful :: (EL.Error () EL.:> es) => Int -> EL.Eff es a
programEffectful = \case
    0 -> EL.throwError ()
    n -> EL.catchError (programEffectful (n - 1)) \_ () -> EL.throwError ()
{-# NOINLINE programEffectful #-}

catchEffectful :: Int -> Either (EL.CallStack, ()) ()
catchEffectful n = EL.runPureEff $ EL.runError $ programEffectful n

catchEffectfulDeep :: Int -> Either (EL.CallStack, ()) ()
catchEffectfulDeep n =
    EL.runPureEff $ run $ run $ run $ run $ run $ EL.runError $ run $ run $ run $ run $ run $ programEffectful n
  where
    run = EL.runReader ()

programEff :: (E.Error () E.:< es) => Int -> E.Eff es a
programEff = \case
    0 -> E.throw ()
    n -> E.catch (programEff (n - 1)) \() -> E.throw ()
{-# NOINLINE programEff #-}

catchEff :: Int -> Either () ()
catchEff n = E.run $ E.runError $ programEff n

catchEffDeep :: Int -> Either () ()
catchEffDeep n = E.run $ run $ run $ run $ run $ run $ E.runError $ run $ run $ run $ run $ run $ programEff n
  where
    run = E.runReader ()

programMtl :: (M.MonadError () m) => Int -> m a
programMtl = \case
    0 -> M.throwError ()
    n -> M.catchError (programMtl (n - 1)) \() -> M.throwError ()
{-# NOINLINE programMtl #-}

catchMtl :: Int -> Either () ()
catchMtl n = M.runExcept $ programMtl n

catchMtlDeep :: Int -> Either () ()
catchMtlDeep n = M.runIdentity $ run $ run $ run $ run $ run $ M.runExceptT $ run $ run $ run $ run $ run $ programMtl n
  where
    run = (`M.runReaderT` ())
