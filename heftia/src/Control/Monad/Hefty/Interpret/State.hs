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

import Control.Effect (Free (liftFree), runAllEff, sendFor, type (~>))
import Control.Effect qualified as D
import Control.Monad.Hefty.Types (Eff, Freer (Op, Val), qApp)
import Data.Effect.HandlerVec (
    FOEs,
    HFunctors,
    HandlerVec,
    KnownOrder,
    Membership,
    Suffix,
    compareMembership,
    generate,
    generateHF,
    hcfmapVec,
    hfmapElem,
    labelMembership,
    suffix,
    weakensFor,
    (!:),
    (:>),
 )
import Data.Kind (Type)
import Data.Type.Equality (type (:~:) (Refl))
import GHC.Generics ((:+:) (L1, R1))

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
reinterpretStateBy s0 ret hdl =
    transEffStateBy s0 ret hdl \v ->
        liftFree . L1 !: generate (suffix v) \i ->
            liftFree . R1 . sendFor (weakensFor i)
{-# INLINE reinterpretStateBy #-}

interpretStateRecWith
    :: forall s e es a
     . (KnownOrder e, HFunctors es)
    => s
    -> (forall ans. StateHandler s e (Eff es) (Eff es) ans)
    -> Eff (e ': es) a
    -> Eff es a
interpretStateRecWith = reinterpretStateRecWith
{-# INLINE interpretStateRecWith #-}

reinterpretStateRecWith
    :: forall s e es' es a
     . (Suffix es es', KnownOrder e, HFunctors es)
    => s
    -> (forall ans. StateHandler s e (Eff es') (Eff es') ans)
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretStateRecWith s0 hdl = loop s0
  where
    loop :: s -> Eff (e ': es) ~> Eff es'
    loop s =
        transEffStateBy s (const pure) (\e s' -> hdl (hfmapElem (loop s') e) s') \v ->
            liftFree . L1 !: hcfmapVec (loop s) (generateHF (suffix v) \i -> liftFree . R1 . sendFor (weakensFor i))
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
interposeStateForBy i s0 ret hdl =
    transEffStateBy s0 ret hdl \v -> generate v \j ->
        liftFree . case compareMembership i j of
            Just Refl -> L1
            Nothing -> R1 . sendFor j
{-# INLINE interposeStateForBy #-}

transEffStateBy
    :: s
    -> (s -> a -> Eff es' ans)
    -> StateHandler s e (Eff es) (Eff es') ans
    -> ( forall r
          . HandlerVec es' (Eff es') (Freer r)
         -> HandlerVec es (Eff es) (Freer (e (Eff es) :+: Eff es'))
       )
    -> Eff es a
    -> Eff es' ans
transEffStateBy s0 ret hdl f (D.Eff m) =
    D.Eff \v -> runAllEff v . delimitState s0 ret hdl . m . f $ v
{-# INLINE transEffStateBy #-}

delimitState
    :: s
    -> (s -> a -> Eff es' ans)
    -> StateHandler s e (Eff es) (Eff es') ans
    -> Freer (e (Eff es) :+: Eff es') a
    -> Eff es' ans
delimitState s0 ret hdl = loop s0
  where
    loop s = \case
        Val x -> ret s x
        Op u q ->
            let k s' = loop s' . qApp q
             in case u of
                    L1 e -> hdl e s k
                    R1 n -> n >>= k s
{-# INLINE delimitState #-}

-- TODO: add other pattern functions.
