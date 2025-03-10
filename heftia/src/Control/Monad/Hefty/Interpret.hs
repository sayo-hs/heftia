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

import Control.Effect (liftFree, runAllEff, sendFor, type (~>))
import Control.Effect qualified as D
import Control.Effect.Interpret hiding (runEff, runPure)
import Control.Effect.Interpret qualified as D
import Control.Monad.Hefty.Types (
    Eff,
    Freer (Op, Val),
    Handler,
    qApp,
 )
import Data.Effect (Emb, Nop)
import Data.Effect.HandlerVec (
    FOEs,
    HFunctors,
    HandlerVec,
    KnownOrder,
    Membership,
    Suffix,
    compareMembership,
    empty,
    generate,
    generateHF,
    hcfmapVec,
    hfmapElem,
    labelMembership,
    suffix,
    vmapVec,
    weakensFor,
    (!:),
    (:>),
    type (++),
 )
import Data.Effect.HandlerVec qualified as V
import Data.Type.Equality (type (:~:) (Refl))
import GHC.Generics (type (:+:) (L1, R1))

-- * Running t`Eff`

-- | Lowers the computation into a monad @m@ by treating the effect as a monad.
runEff :: (Monad m) => Eff '[Emb m] ~> m
runEff = D.runEff
{-# INLINE runEff #-}

-- | Extracts the value from a computation that contains only pure values without any effect.
runPure :: Eff '[] a -> a
runPure (D.Eff m) =
    case m @(Nop _) empty of
        Val x -> x
        Op r _ -> case r of {}
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

interpretsBy
    :: forall es r ans a
     . (FOEs r)
    => (a -> Eff r ans)
    -> Handlers es (Eff (es ++ r)) (Eff r) ans
    -> Eff (es ++ r) a
    -> Eff r ans
interpretsBy = reinterpretsBy @_ @r
{-# INLINE interpretsBy #-}

-- Handler bundle

type Handlers es m n ans = HandlerVec es m (HandlerCC n ans)

newtype HandlerCC n ans x = HandlerCC {getHandlerCC :: (x -> n ans) -> n ans}

infixr 5 !::
(!::) :: (KnownOrder e) => Handler e m n ans -> Handlers es m n ans -> Handlers (e ': es) m n ans
h !:: v = HandlerCC . h !: v
{-# INLINE (!::) #-}

--

{- | Interprets the effect @e@ at the head of the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unhandled higher-order effects.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond the scopes.
-}
interpretRecWith
    :: forall e es a
     . (KnownOrder e, HFunctors es)
    => (forall ans. Handler e (Eff es) (Eff es) ans)
    -> Eff (e ': es) a
    -> Eff es a
interpretRecWith = reinterpretRecWith
{-# INLINE interpretRecWith #-}

reinterpretWith
    :: forall e es' es a
     . (Suffix es es', KnownOrder e, FOEs es)
    => Handler e (Eff (e ': es)) (Eff es') a
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretWith = reinterpretBy pure
{-# INLINE reinterpretWith #-}

reinterpretBy
    :: forall e es es' ans a
     . (KnownOrder e, FOEs es, Suffix es es')
    => (a -> Eff es' ans)
    -> Handler e (Eff (e ': es)) (Eff es') ans
    -> Eff (e ': es) a
    -> Eff es' ans
reinterpretBy ret hdl =
    transEffBy ret hdl \v ->
        liftFree . L1 !: generate (suffix v) \i ->
            liftFree . R1 . sendFor (weakensFor i)
{-# INLINE reinterpretBy #-}

reinterpretsBy
    :: forall es r r' ans a
     . (FOEs r, Suffix r r')
    => (a -> Eff r' ans)
    -> Handlers es (Eff (es ++ r)) (Eff r') ans
    -> Eff (es ++ r) a
    -> Eff r' ans
reinterpretsBy ret hdl =
    transEffsBy ret \v ->
        vmapVec (liftFree . L1) hdl
            `V.concat` generate @r (suffix v) \i ->
                liftFree . R1 . sendFor (weakensFor i)
{-# INLINE reinterpretsBy #-}

reinterpretRecWith
    :: forall e es' es a
     . (Suffix es es', KnownOrder e, HFunctors es)
    => (forall ans. Handler e (Eff es') (Eff es') ans)
    -> Eff (e ': es) a
    -> Eff es' a
reinterpretRecWith hdl = loop
  where
    loop :: Eff (e ': es) ~> Eff es'
    loop = transEffBy pure (hdl . hfmapElem loop) \v ->
        liftFree . L1 !: hcfmapVec loop (generateHF (suffix v) \i -> liftFree . R1 . sendFor (weakensFor i))
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
     . (e :> es, KnownOrder e, HFunctors es)
    => (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeRecWith = interposeRecForWith labelMembership
{-# INLINE interposeRecWith #-}

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
    transEffBy ret hdl \v -> generate v \j ->
        liftFree . case compareMembership i j of
            Just Refl -> L1
            Nothing -> R1 . sendFor j
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
     . (KnownOrder e, HFunctors es)
    => Membership e es
    -> (forall ans. Handler e (Eff es) (Eff es) ans)
    -- ^ Effect handler
    -> Eff es a
    -> Eff es a
interposeRecForWith i hdl = loop
  where
    loop :: Eff es ~> Eff es
    loop =
        transEffBy pure hdl \v ->
            hcfmapVec loop $ generateHF v \j ->
                liftFree . case compareMembership i j of
                    Just Refl -> L1
                    Nothing -> R1 . sendFor j
{-# INLINE interposeRecForWith #-}

{- TODO: add the patterns:
    - interpose{In,On}By
    - interpose{In,On}With
    - interposeRec{In,On}With
    - preinterposeRec{,In,On,For}With
-}

transEffsBy
    :: (a -> Eff es' ans)
    -> ( forall r
          . HandlerVec es' (Eff es') (Freer r)
         -> HandlerVec es (Eff es) (Freer (HandlerCC (Eff es') ans :+: Eff es'))
       )
    -> Eff es a
    -> Eff es' ans
transEffsBy ret f (D.Eff m) =
    D.Eff \v -> runAllEff v . delimits ret . m . f $ v
{-# INLINE transEffsBy #-}

delimits
    :: (a -> Eff es' ans)
    -> Freer (HandlerCC (Eff es') ans :+: Eff es') a
    -> Eff es' ans
delimits ret = loop
  where
    loop = \case
        Val x -> ret x
        Op s q ->
            let k = loop . qApp q
             in case s of
                    L1 (HandlerCC initiate) -> initiate k
                    R1 n -> n >>= k
{-# INLINE delimits #-}

transEffBy
    :: (a -> Eff es' ans)
    -> Handler e (Eff es) (Eff es') ans
    -> ( forall r
          . HandlerVec es' (Eff es') (Freer r)
         -> HandlerVec es (Eff es) (Freer (e (Eff es) :+: Eff es'))
       )
    -> Eff es a
    -> Eff es' ans
transEffBy ret hdl f (D.Eff m) =
    D.Eff \v -> runAllEff v . delimit ret hdl . m . f $ v
{-# INLINE transEffBy #-}

delimit
    :: (a -> Eff es' ans)
    -> Handler e (Eff es) (Eff es') ans
    -> Freer (e (Eff es) :+: Eff es') a
    -> Eff es' ans
delimit ret hdl = loop
  where
    loop = \case
        Val x -> ret x
        Op s q ->
            let k = loop . qApp q
             in case s of
                    L1 e -> hdl e k
                    R1 n -> n >>= k
{-# INLINE delimit #-}

-- * Utilities

-- | Lifts a stateless handler into a continuational stateful handler.
stateless :: forall e m n ans. (Monad n) => (e m ~> n) -> Handler e m n ans
stateless i e k = i e >>= k
{-# INLINE stateless #-}
