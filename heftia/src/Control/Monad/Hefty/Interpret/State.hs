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

import Control.Effect (unEff, type (~>))
import Control.Monad.Hefty.Types (Eff, Freer (Op, Val), qApp, sendUnionBy)
import Data.Effect.HFunctor (hfmap)
import Data.Effect.OpenUnion (FOEs, KnownOrder, Membership, Union, Weaken, coerceFOEs, labelMembership, project, weakens, (!+), (:>))
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
     . (Weaken es es', KnownOrder e, FOEs es)
    => s
    -> (s -> a -> Eff es' ans)
    -> StateHandler s e (Eff (e ': es)) (Eff es') ans
    -> Eff (e ': es) a
    -> Eff es' ans
reinterpretStateBy s0 ret hdl =
    iterStateAllEffBy s0 ret (hdl !+ \u s k -> sendUnionBy (k s) (weakens $ coerceFOEs u))
{-# INLINE reinterpretStateBy #-}

interpretStateRecWith
    :: forall s e es a
     . (KnownOrder e)
    => s
    -> (forall ans. StateHandler s e (Eff (e ': es)) (Eff es) ans)
    -> Eff (e ': es) a
    -> Eff es a
interpretStateRecWith = reinterpretStateRecWith
{-# INLINE interpretStateRecWith #-}

reinterpretStateRecWith
    :: forall s e es' es a
     . (Weaken es es', KnownOrder e)
    => s
    -> (forall ans. StateHandler s e (Eff (e ': es)) (Eff es') ans)
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretStateRecWith s0 hdl = loop s0
  where
    loop :: s -> Eff (e ': es) ~> Eff es'
    loop s =
        iterStateAllEffBy
            s
            (const pure)
            (hdl !+ \u s' k -> sendUnionBy (k s') (weakens $ hfmap (loop s') u))
{-# INLINE reinterpretStateRecWith #-}

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

interposeStateForBy
    :: forall s e es ans a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> s
    -> (s -> a -> Eff es ans)
    -> StateHandler s e (Eff es) (Eff es) ans
    -> Eff es a
    -> Eff es ans
interposeStateForBy i s0 ret f =
    iterStateAllEffBy s0 ret \u s k ->
        maybe (sendUnionBy (k s) u) (\e -> f e s k) (project i u)
{-# INLINE interposeStateForBy #-}

-- * Transformation to monads

iterStateAllEffBy
    :: forall s es m ans a
     . (Monad m)
    => s
    -> (s -> a -> m ans)
    -> StateHandler s (Union es) (Eff es) m ans
    -> Eff es a
    -> m ans
iterStateAllEffBy s0 ret hdl = loop s0 . unEff
  where
    loop s = \case
        Val x -> ret s x
        Op u q -> hdl u s k
          where
            k s' = loop s' . qApp q
{-# INLINE iterStateAllEffBy #-}

-- TODO: add other pattern functions.
