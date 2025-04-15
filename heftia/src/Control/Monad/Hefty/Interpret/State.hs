-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

{- |
Copyright   :  (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King; 2024 Sayo contributors
License     :  MPL-2.0 AND BSD-3-Clause (see the LICENSE and LICENSE.BSD3 files)
Maintainer  :  ymdfield@outlook.jp

This module provides an ad-hoc specialized version of
 "Control.Monad.Hefty.Interpret" to accelerate interpretations that have a
single state type @s@, especially for effects like t'Data.Effect.State.State' or
 [@Writer@]("Data.Effect.Writer").
-}
module Control.Monad.Hefty.Interpret.State where

import Control.Effect (unEff)
import Control.Effect qualified as D
import Control.Monad.Hefty.Types (Eff, Freer (Op, Val), qApp)
import Data.Effect.OpenUnion (
    FOEs,
    In,
    KnownOrder,
    Membership,
    Suffix,
    coerceFOEs,
    identityMembership,
    labelMembership,
    project,
    weakens,
    (!:),
    (:>),
 )
import Data.FTCQueue (tsingleton)
import Data.Function ((&))
import Data.Kind (Type)

-- | An ad-hoc stateful version of t'Control.Monad.Hefty.Types.Handler' for performance.
type StateHandler s e m n (ans :: Type) = forall x. e m x -> s -> (s -> x -> n ans) -> n ans

-- * Interpretation functions

interpretStateBy
    :: forall s e es ans a
     . (KnownOrder e, FOEs es)
    => s
    -> (s -> a -> Eff es ans)
    -> StateHandler s e (Eff (e ': es)) (Eff es) ans
    -> Eff (e ': es) a
    -> Eff es ans
interpretStateBy = reinterpretStateBy
{-# INLINE interpretStateBy #-}

reinterpretStateBy
    :: forall s e es' es ans a
     . (Suffix es es', KnownOrder e, FOEs es)
    => s
    -> (s -> a -> Eff es' ans)
    -> StateHandler s e (Eff (e ': es)) (Eff es') ans
    -> Eff (e ': es) a
    -> Eff es' ans
reinterpretStateBy s0 ret hdl = loop s0
  where
    loop s (D.Eff m) = case m of
        Val x -> ret s x
        Op u q ->
            let k s' = loop s' . D.Eff . qApp q
             in u & (\e -> hdl e s k) !: D.Eff . (`Op` (tsingleton $ unEff . k s0)) . weakens . coerceFOEs
{-# INLINE reinterpretStateBy #-}

-- * Interposition functions

interposeStateBy
    :: forall s e es ans a
     . (e :> es, FOEs es)
    => s
    -> (s -> a -> Eff es ans)
    -> StateHandler s e (Eff es) (Eff es) ans
    -> Eff es a
    -> Eff es ans
interposeStateBy = interposeStateForBy labelMembership
{-# INLINE interposeStateBy #-}

interposeStateInBy
    :: forall s e es ans a
     . (e `In` es, FOEs es)
    => s
    -> (s -> a -> Eff es ans)
    -> StateHandler s e (Eff es) (Eff es) ans
    -> Eff es a
    -> Eff es ans
interposeStateInBy = interposeStateForBy identityMembership
{-# INLINE interposeStateInBy #-}

interposeStateForBy
    :: forall s e es ans a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> s
    -> (s -> a -> Eff es ans)
    -> StateHandler s e (Eff es) (Eff es) ans
    -> Eff es a
    -> Eff es ans
interposeStateForBy i s0 ret hdl = loop s0
  where
    loop s (D.Eff m) = case m of
        Val x -> ret s x
        Op u q ->
            let k s' = loop s' . D.Eff . qApp q
             in case project i u of
                    Just e -> hdl e s k
                    Nothing -> D.Eff $ Op u (tsingleton $ unEff . k s0)
{-# INLINE interposeStateForBy #-}

-- TODO: add other pattern functions.
