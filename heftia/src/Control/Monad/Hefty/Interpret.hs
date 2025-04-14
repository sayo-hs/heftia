{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

{- |
Copyright   :  (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King; 2024 Sayo contributors
License     :  MPL-2.0 AND BSD-3-Clause (see the LICENSE and LICENSE.BSD3 files)
Maintainer  :  ymdfield@outlook.jp

This module provides functions for interpretation.
Please refer to the documentation of the [top-level module]("Control.Monad.Hefty").
-}
module Control.Monad.Hefty.Interpret (
    module Control.Monad.Hefty.Interpret,
    module Control.Effect.Interpret,
)
where

import Control.Effect (unEff, type (~>))
import Control.Effect qualified as D
import Control.Effect.Interpret hiding (runEff, runPure)
import Control.Effect.Interpret qualified as D
import Control.Monad.Hefty.Types (
    AlgHandler,
    Eff,
    Freer (Op, Val),
    qApp,
 )
import Data.Effect (Emb)
import Data.Effect.OpenUnion (
    FOEs,
    KnownLength,
    KnownOrder,
    Membership,
    Suffix,
    Union,
    coerceFOEs,
    labelMembership,
    nil,
    project,
    weakens,
    (!++),
    (!:),
    (:>),
    type (++),
 )
import Data.FTCQueue (tsingleton)
import Data.Function ((&))

-- * Running t`Eff`

-- | Lowers the computation into a monad @m@ by treating the effect as a monad.
runEff :: (Monad m) => Eff '[Emb m] ~> m
runEff = D.runEff
{-# INLINE runEff #-}

-- | Extracts the value from a computation that contains only pure values without any effect.
runPure :: Eff '[] a -> a
runPure (D.Eff m) =
    case m of
        Val x -> x
        Op r _ -> nil r
{-# INLINE runPure #-}

-- * Standard continuational interpretation functions

-- | Interprets the effect @e@ at the head of the list using the provided continuational stateful handler.
interpretWith
    :: forall e es a
     . (KnownOrder e, FOEs es)
    => AlgHandler e (Eff (e ': es)) (Eff es) a
    -> Eff (e ': es) a
    -> Eff es a
interpretWith = reinterpretWith
{-# INLINE interpretWith #-}

-- | Interprets the effect @e@ at the head of the list using the provided value handler and continuational stateful handler.
interpretBy
    :: forall e es ans a
     . (KnownOrder e, FOEs es)
    => (a -> Eff es ans)
    -> AlgHandler e (Eff (e ': es)) (Eff es) ans
    -> Eff (e ': es) a
    -> Eff es ans
interpretBy = reinterpretBy
{-# INLINE interpretBy #-}

interpretsBy
    :: forall es r ans a
     . (FOEs r, KnownLength es)
    => (a -> Eff r ans)
    -> AlgHandler (Union es) (Eff (es ++ r)) (Eff r) ans
    -> Eff (es ++ r) a
    -> Eff r ans
interpretsBy = reinterpretsBy @_ @r
{-# INLINE interpretsBy #-}

reinterpretWith
    :: forall e es' es a
     . (Suffix es es', KnownOrder e, FOEs es)
    => AlgHandler e (Eff (e ': es)) (Eff es') a
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretWith = reinterpretBy pure
{-# INLINE reinterpretWith #-}

reinterpretBy
    :: forall e es es' ans a
     . (KnownOrder e, FOEs es, Suffix es es')
    => (a -> Eff es' ans)
    -> AlgHandler e (Eff (e ': es)) (Eff es') ans
    -> Eff (e ': es) a
    -> Eff es' ans
reinterpretBy ret hdl = loop
  where
    loop (D.Eff m) = case m of
        Val x -> ret x
        Op u q ->
            let k = loop . D.Eff . qApp q
             in u & (`hdl` k) !: D.Eff . (`Op` tsingleton (unEff . k)) . weakens . coerceFOEs
{-# INLINE reinterpretBy #-}

reinterpretsBy
    :: forall es r r' ans a
     . (FOEs r, Suffix r r', KnownLength es)
    => (a -> Eff r' ans)
    -> AlgHandler (Union es) (Eff (es ++ r)) (Eff r') ans
    -> Eff (es ++ r) a
    -> Eff r' ans
reinterpretsBy ret hdl = loop
  where
    loop :: Eff (es ++ r) a -> Eff r' ans
    loop (D.Eff m) = case m of
        Val x -> ret x
        Op u q ->
            let k = loop . D.Eff . qApp q
             in u & (`hdl` k) !++ D.Eff . (`Op` tsingleton (unEff . k)) . weakens @r . coerceFOEs
{-# INLINE reinterpretsBy #-}

-- * Interposition functions

-- | Reinterprets (hooks) the effect @e@ in the list using the provided value handler and continuational stateful handler.
interposeBy
    :: forall e es ans a
     . (e :> es, FOEs es)
    => (a -> Eff es ans)
    -- ^ Value handler
    -> AlgHandler e (Eff es) (Eff es) ans
    -- ^ Effect handler
    -> Eff es a
    -> Eff es ans
interposeBy = interposeForBy labelMembership
{-# INLINE interposeBy #-}

-- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.
interposeWith
    :: forall e es a
     . (e :> es, FOEs es)
    => AlgHandler e (Eff es) (Eff es) a
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeWith = interposeForWith labelMembership
{-# INLINE interposeWith #-}

-- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.
interposeForWith
    :: forall e es a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> AlgHandler e (Eff es) (Eff es) a
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeForWith i = interposeForBy i pure
{-# INLINE interposeForWith #-}

{- TODO: add the patterns:
    - interpose{In,On}By
    - interpose{In,On}With
-}

interposeForBy
    :: forall e es ans a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> (a -> Eff es ans)
    -- ^ Value handler
    -> AlgHandler e (Eff es) (Eff es) ans
    -- ^ Effect handler
    -> Eff es a
    -> Eff es ans
interposeForBy i ret hdl = loop
  where
    loop (D.Eff a) = case a of
        Val x -> ret x
        Op u q ->
            let k = loop . D.Eff . qApp q
             in case project i u of
                    Just e -> hdl e k
                    Nothing -> D.Eff $ Op u (tsingleton $ unEff . k)
{-# INLINE interposeForBy #-}

-- * Utilities

-- | Lifts a stateless handler into a continuational stateful handler.
stateless :: forall e m n ans. (Monad n) => (e m ~> n) -> AlgHandler e m n ans
stateless i e k = i e >>= k
{-# INLINE stateless #-}
