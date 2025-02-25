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

import Control.Arrow ((>>>))
import Control.Effect (hoist, unEff, type (~>))
import Control.Effect qualified as D
import Control.Effect.Interpret hiding (runEff, runPure)
import Control.Effect.Interpret qualified as D
import Control.Monad.Hefty.Types (
    Eff,
    Freer (Op, Val),
    Handler,
    qApp,
    sendUnionBy,
 )
import Data.Effect (Emb)
import Data.Effect.HFunctor (hfmap)
import Data.Effect.OpenUnion (
    Elem (extract, project, (!+)),
    FOEs,
    KnownOrder,
    Membership,
    Union,
    Weaken,
    coerceFOEs,
    labelMembership,
    nil,
    weakens,
    type (:>),
 )
import Data.FTCQueue (tsingleton)
import Data.Functor ((<&>))

-- * Running t`Eff`

-- | Lowers the computation into a monad @m@ by treating the effect as a monad.
runEff :: (Monad m) => Eff '[Emb m] ~> m
runEff = D.runEff
{-# INLINE runEff #-}

-- | Extracts the value from a computation that contains only pure values without any effect.
runPure :: Eff '[] a -> a
runPure =
    unEff >>> \case
        Val x -> x
        Op u _ -> nil u
{-# INLINE runPure #-}

-- * Standard continuational interpretation functions

-- | Interprets the effect @e@ at the head of the list using the provided continuational stateful handler.
interpretWith
    :: forall e es a
     . (KnownOrder e, FOEs es)
    => Handler e (Eff (e ': es)) (Eff es) a
    -> Eff (e ': es) a
    -> Eff es a
interpretWith = reinterpretWith
{-# INLINE interpretWith #-}

-- | Interprets the effect @e@ at the head of the list using the provided value handler and continuational stateful handler.
interpretBy
    :: forall e es ans a
     . (KnownOrder e, FOEs es)
    => (a -> Eff es ans)
    -> Handler e (Eff (e ': es)) (Eff es) ans
    -> Eff (e ': es) a
    -> Eff es ans
interpretBy = reinterpretBy
{-# INLINE interpretBy #-}

{- | Interprets the effect @e@ at the head of the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unhandled higher-order effects.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond the scopes.
-}
interpretRecWith
    :: forall e es a
     . (KnownOrder e)
    => (forall ans. Handler e (Eff es) (Eff es) ans)
    -> Eff (e ': es) a
    -> Eff es a
interpretRecWith = reinterpretRecWith
{-# INLINE interpretRecWith #-}

reinterpretWith
    :: forall e es' es a
     . (Weaken es es', KnownOrder e, FOEs es)
    => Handler e (Eff (e ': es)) (Eff es') a
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretWith = reinterpretBy pure
{-# INLINE reinterpretWith #-}

reinterpretBy
    :: forall e es' es ans a
     . (Weaken es es', KnownOrder e, FOEs es)
    => (a -> Eff es' ans)
    -> Handler e (Eff (e ': es)) (Eff es') ans
    -> Eff (e ': es) a
    -> Eff es' ans
reinterpretBy ret hdl = iterAllEffBy ret (hdl !+ flip sendUnionBy . weakens . coerceFOEs)
{-# INLINE reinterpretBy #-}

reinterpretRecWith
    :: forall e es' es a
     . (Weaken es es', KnownOrder e)
    => (forall ans. Handler e (Eff es') (Eff es') ans)
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretRecWith hdl = loop
  where
    loop :: Eff (e ': es) ~> Eff es'
    loop = iterAllEffBy pure ((hdl !+ flip sendUnionBy . weakens) . hfmap loop)
{-# INLINE reinterpretRecWith #-}

-- * Interposition functions

-- | Reinterprets (hooks) the effect @e@ in the list using the provided value handler and continuational stateful handler.
interposeBy
    :: forall e es ans a
     . (e :> es, FOEs es)
    => (a -> Eff es ans)
    -- ^ Value handler
    -> Handler e (Eff es) (Eff es) ans
    -- ^ Effect handler
    -> Eff es a
    -> Eff es ans
interposeBy = interposeForBy labelMembership
{-# INLINE interposeBy #-}

-- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.
interposeWith
    :: forall e es a
     . (e :> es, FOEs es)
    => Handler e (Eff es) (Eff es) a
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeWith = interposeForWith labelMembership
{-# INLINE interposeWith #-}

{- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unhandled higher-order effects.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond the scopes.
-}
interposeRecWith
    :: forall e es a
     . (e :> es, KnownOrder e)
    => (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeRecWith = interposeRecForWith labelMembership
{-# INLINE interposeRecWith #-}

preinterposeRecWith
    :: forall e es a
     . (e :> es)
    => (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
preinterposeRecWith = preinterposeRecForWith labelMembership
{-# INLINE preinterposeRecWith #-}

-- | Reinterprets (hooks) the effect @e@ in the list using the provided value handler and continuational stateful handler.
interposeForBy
    :: forall e es ans a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> (a -> Eff es ans)
    -- ^ Value handler
    -> Handler e (Eff es) (Eff es) ans
    -- ^ Effect handler
    -> Eff es a
    -> Eff es ans
interposeForBy i ret hdl =
    iterAllEffBy ret \u -> maybe (`sendUnionBy` u) hdl (project i u)
{-# INLINE interposeForBy #-}

-- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.
interposeForWith
    :: forall e es a
     . (KnownOrder e, FOEs es)
    => Membership e es
    -> Handler e (Eff es) (Eff es) a
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeForWith i = interposeForBy i pure
{-# INLINE interposeForWith #-}

{- | Reinterprets (hooks) the effect @e@ in the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unhandled higher-order effects.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond the scopes.
-}
interposeRecForWith
    :: forall e es a
     . (KnownOrder e)
    => Membership e es
    -> (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeRecForWith i hdl = loop
  where
    loop :: Eff es ~> Eff es
    loop =
        iterAllEffBy
            pure
            (hfmap loop >>> \u -> maybe (`sendUnionBy` u) hdl (project i u))
{-# INLINE interposeRecForWith #-}

preinterposeRecForWith
    :: forall e es a
     . (KnownOrder e)
    => Membership e es
    -> (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
preinterposeRecForWith i hdl = loop
  where
    loop :: Eff es ~> Eff es
    loop =
        iterAllEffBy
            pure
            ( ((D.Eff . hoist (hfmap loop) . unEff) .)
                . \u -> maybe (`sendUnionBy` u) hdl (project i u)
            )
{-# INLINE preinterposeRecForWith #-}

{- TODO: add the patterns:
    - interpose{In,On}By
    - interpose{In,On}With
    - interposeRec{In,On}With
    - preinterposeRec{In,On}With
-}

-- * Transformation to monads

iterEffBy
    :: forall e m ans a
     . (Monad m, KnownOrder e)
    => (a -> m ans)
    -- ^ Value handler
    -> Handler e (Eff '[e]) m ans
    -- ^ Effect handler
    -> Eff '[e] a
    -> m ans
iterEffBy ret hdl = iterAllEffBy ret (hdl . extract)
{-# INLINE iterEffBy #-}

iterAllEffBy
    :: forall es m ans a
     . (Monad m)
    => (a -> m ans)
    -- ^ Value handler
    -> Handler (Union es) (Eff es) m ans
    -- ^ Effect handler
    -> Eff es a
    -> m ans
iterAllEffBy ret hdl = loop . unEff
  where
    loop = \case
        Val x -> ret x
        Op u q -> hdl u k
          where
            k = loop . qApp q
{-# INLINE iterAllEffBy #-}

-- * Utilities

-- | Lifts a stateless handler into a continuational stateful handler.
stateless :: forall e m n ans. (Monad n) => (e m ~> n) -> Handler e m n ans
stateless i e k = i e >>= k
{-# INLINE stateless #-}

interleave :: Eff es a -> Eff es b -> Eff es (a, b)
interleave (D.Eff (Val x)) m = (x,) <$> m
interleave m (D.Eff (Val x)) = m <&> (,x)
interleave (D.Eff (Op u k)) (D.Eff (Op u' k')) = do
    x <- D.Eff $ Op u (tsingleton pure)
    y <- D.Eff $ Op u' (tsingleton pure)
    interleave (D.Eff $ qApp k x) (D.Eff $ qApp k' y)
