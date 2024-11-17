{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King; 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file) AND BSD-3-Clause (see the LICENSE.BSD3 file)
Maintainer  :  ymdfield@outlook.jp

This module provides functions for interpretation.
Please refer to the documentation of the [top-level module]("Control.Monad.Hefty").
-}
module Control.Monad.Hefty.Interpret where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Monad ((>=>))
import Control.Monad.Hefty.Types (
    Eff (Op, Val),
    Elaborator,
    Interpreter,
    send0,
    sendUnion,
    sendUnionBy,
    sendUnionH,
    sendUnionHBy,
    type (~~>),
 )
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.OpenUnion.Internal (IsSuffixOf, WeakenN)
import Data.Effect.OpenUnion.Internal.FO (
    Member (prj),
    Union,
    extract,
    nil,
    weakenN,
    weakens,
    (!+),
    type (<|),
 )
import Data.Effect.OpenUnion.Internal.HO (
    MemberH (prjH),
    UnionH,
    extractH,
    hfmapUnion,
    nilH,
    weakenNH,
    weakensH,
    (!!+),
    type (<<|),
 )
import Data.FTCQueue (FTCQueue, ViewL (TOne, (:|)), tsingleton, tviewl, (><))
import Data.Functor ((<&>))

-- * Running t`Eff`

-- | Lowers the computation into a monad @m@ by treating the effect as a monad.
runEff :: (Monad m) => Eff '[] '[m] ~> m
runEff = iterEffBy pure $ stateless id
{-# INLINE runEff #-}

-- | Extracts the value from a computation that contains only pure values without any effect.
runPure :: Eff '[] '[] a -> a
runPure = \case
    Val x -> x
    Op u _ -> case u of
        Left u' -> nilH u'
        Right u' -> nil u'
{-# INLINE runPure #-}

-- * Standard interpretation functions

-- ** For first-order effects

-- | Interprets the first-order effect @e@ at the head of the list using the provided natural transformation style handler.
interpret
    :: forall e ef eh
     . (e ~> Eff eh ef)
    -- ^ Effect handler
    -> Eff eh (e ': ef) ~> Eff eh ef
interpret = reinterpret
{-# INLINE interpret #-}

-- | Interprets the first-order effect @e@ at the head of the list using the provided continuational stateful handler.
interpretWith
    :: forall e ef a
     . Interpreter e (Eff '[] ef) a
    -- ^ Effect handler
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef a
interpretWith = reinterpretWith
{-# INLINE interpretWith #-}

-- | Interprets the first-order effect @e@ at the head of the list using the provided value handler and continuational stateful handler.
interpretBy
    :: forall e ef ans a
     . (a -> Eff '[] ef ans)
    -- ^ Value handler
    -> Interpreter e (Eff '[] ef) ans
    -- ^ Effect handler
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef ans
interpretBy = reinterpretBy
{-# INLINE interpretBy #-}

{- | Interprets the first-order effect @e@ at the head of the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unelaborated higher-order effects @eh@.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond scopes.
-}
interpretRecWith
    :: forall e ef eh a
     . (forall ans. Interpreter e (Eff eh ef) ans)
    -- ^ Effect handler
    -> Eff eh (e ': ef) a
    -> Eff eh ef a
interpretRecWith = reinterpretRecWith
{-# INLINE interpretRecWith #-}

-- ** For higher-order effects

-- | Interprets the higher-order effect @e@ at the head of the list using the provided natural transformation style elaborator.
interpretH
    :: forall e eh ef
     . (HFunctor e)
    => e ~~> Eff eh ef
    -- ^ Effect elaborator
    -> Eff (e ': eh) ef ~> Eff eh ef
interpretH = reinterpretH
{-# INLINE interpretH #-}

-- | Interprets the single higher-order effect @e@ using the provided continuational stateful elaborator.
interpretHWith
    :: forall e eh ef a
     . (HFunctor e)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -- ^ Effect elaborator
    -> Eff '[e] ef a
    -> Eff eh ef a
interpretHWith = reinterpretHWith
{-# INLINE interpretHWith #-}

-- | Interprets the single higher-order effect @e@ using the provided value handler and continuational stateful elaborator.
interpretHBy
    :: forall e eh ef ans a
     . (HFunctor e)
    => (a -> Eff eh ef ans)
    -- ^ Value handler
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -- ^ Effect elaborator
    -> Eff '[e] ef a
    -> Eff eh ef ans
interpretHBy = reinterpretHBy
{-# INLINE interpretHBy #-}

{- | Interprets the higher-order effect @e@ at the head of the list using the provided continuational stateful elaborator.

Interpretation is performed recursively with respect to the scopes of unelaborated higher-order effects @eh@.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond scopes.
-}
interpretRecHWith
    :: forall e eh ef a
     . (HFunctor e)
    => (forall ans. Elaborator e (Eff eh ef) ans)
    -- ^ Effect elaborator
    -> Eff (e ': eh) ef a
    -> Eff eh ef a
interpretRecHWith = reinterpretRecHWith
{-# INLINE interpretRecHWith #-}

-- * Reinterpretation functions

-- ** For first-order effects

reinterpret
    :: forall e ef' ef eh
     . (ef `IsSuffixOf` ef')
    => (e ~> Eff eh ef')
    -> Eff eh (e ': ef) ~> Eff eh ef'
reinterpret f = reinterpretRecWith (stateless f)
{-# INLINE reinterpret #-}

reinterpretN
    :: forall n e ef' ef eh
     . (WeakenN n ef ef')
    => (e ~> Eff eh ef')
    -> Eff eh (e ': ef) ~> Eff eh ef'
reinterpretN f = reinterpretRecNWith @n (stateless f)
{-# INLINE reinterpretN #-}

reinterpretWith
    :: forall e ef' ef eh a
     . (ef `IsSuffixOf` ef')
    => Interpreter e (Eff eh ef') a
    -> Eff '[] (e ': ef) a
    -> Eff eh ef' a
reinterpretWith = reinterpretBy pure
{-# INLINE reinterpretWith #-}

reinterpretNWith
    :: forall n e ef' ef eh a
     . (WeakenN n ef ef')
    => Interpreter e (Eff eh ef') a
    -> Eff '[] (e ': ef) a
    -> Eff eh ef' a
reinterpretNWith = reinterpretNBy @n pure
{-# INLINE reinterpretNWith #-}

reinterpretBy
    :: forall e ef' ef eh ans a
     . (ef `IsSuffixOf` ef')
    => (a -> Eff eh ef' ans)
    -> Interpreter e (Eff eh ef') ans
    -> Eff '[] (e ': ef) a
    -> Eff eh ef' ans
reinterpretBy ret hdl = iterAllEffHFBy ret nilH (hdl !+ flip sendUnionBy . weakens)
{-# INLINE reinterpretBy #-}

reinterpretNBy
    :: forall n e ef' ef eh ans a
     . (WeakenN n ef ef')
    => (a -> Eff eh ef' ans)
    -> Interpreter e (Eff eh ef') ans
    -> Eff '[] (e ': ef) a
    -> Eff eh ef' ans
reinterpretNBy ret hdl = iterAllEffHFBy ret nilH (hdl !+ flip sendUnionBy . weakenN @n)
{-# INLINE reinterpretNBy #-}

reinterpretRecWith
    :: forall e ef' ef eh a
     . (ef `IsSuffixOf` ef')
    => (forall ans. Interpreter e (Eff eh ef') ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef' a
reinterpretRecWith hdl = loop
  where
    loop :: Eff eh (e ': ef) ~> Eff eh ef'
    loop = iterAllEffHFBy pure (flip sendUnionHBy . hfmapUnion loop) (hdl !+ flip sendUnionBy . weakens)
{-# INLINE reinterpretRecWith #-}

reinterpretRecNWith
    :: forall n e ef' ef eh a
     . (WeakenN n ef ef')
    => (forall ans. Interpreter e (Eff eh ef') ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef' a
reinterpretRecNWith hdl = loop
  where
    loop :: Eff eh (e ': ef) ~> Eff eh ef'
    loop = iterAllEffHFBy pure (flip sendUnionHBy . hfmapUnion loop) (hdl !+ flip sendUnionBy . weakenN @n)
{-# INLINE reinterpretRecNWith #-}

-- ** For higher-order effects

reinterpretH
    :: forall e eh eh' ef
     . (HFunctor e, eh `IsSuffixOf` eh')
    => e ~~> Eff eh' ef
    -> Eff (e ': eh) ef ~> Eff eh' ef
reinterpretH elb = reinterpretRecHWith (stateless elb)
{-# INLINE reinterpretH #-}

reinterpretNH
    :: forall n e eh eh' ef
     . (HFunctor e, WeakenN n eh eh')
    => e ~~> Eff eh' ef
    -> Eff (e ': eh) ef ~> Eff eh' ef
reinterpretNH elb = reinterpretRecNHWith @n (stateless elb)
{-# INLINE reinterpretNH #-}

reinterpretHWith
    :: forall e eh ef a
     . (HFunctor e)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -> Eff '[e] ef a
    -> Eff eh ef a
reinterpretHWith = reinterpretHBy pure
{-# INLINE reinterpretHWith #-}

reinterpretNHWith
    :: forall n e eh ef a
     . (HFunctor e, WeakenN n '[] eh)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -> Eff '[e] ef a
    -> Eff eh ef a
reinterpretNHWith = reinterpretHWith
{-# INLINE reinterpretNHWith #-}

reinterpretHBy
    :: forall e eh ef ans a
     . (HFunctor e)
    => (a -> Eff eh ef ans)
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -> Eff '[e] ef a
    -> Eff eh ef ans
reinterpretHBy ret elb = iterAllEffHFBy ret (elb . extractH) (flip sendUnionBy)
{-# INLINE reinterpretHBy #-}

reinterpretNHBy
    :: forall n e eh ef ans a
     . (HFunctor e, WeakenN n '[] eh)
    => (a -> Eff eh ef ans)
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -> Eff '[e] ef a
    -> Eff eh ef ans
reinterpretNHBy = reinterpretHBy
{-# INLINE reinterpretNHBy #-}

reinterpretRecHWith
    :: forall e eh eh' ef a
     . (HFunctor e, eh `IsSuffixOf` eh')
    => (forall ans. Elaborator e (Eff eh' ef) ans)
    -> Eff (e ': eh) ef a
    -> Eff eh' ef a
reinterpretRecHWith elb = loop
  where
    loop :: Eff (e ': eh) ef ~> Eff eh' ef
    loop =
        iterAllEffHFBy
            pure
            (elb . hfmap loop !!+ flip sendUnionHBy . weakensH . hfmapUnion loop)
            (flip sendUnionBy)
{-# INLINE reinterpretRecHWith #-}

reinterpretRecNHWith
    :: forall n e eh eh' ef a
     . (HFunctor e, WeakenN n eh eh')
    => (forall ans. Elaborator e (Eff eh' ef) ans)
    -> Eff (e ': eh) ef a
    -> Eff eh' ef a
reinterpretRecNHWith elb = loop
  where
    loop :: Eff (e ': eh) ef ~> Eff eh' ef
    loop =
        iterAllEffHFBy
            pure
            (elb . hfmap loop !!+ flip sendUnionHBy . weakenNH @n . hfmapUnion loop)
            (flip sendUnionBy)
{-# INLINE reinterpretRecNHWith #-}

-- * Interposition functions

-- ** For first-order effects

{- | Reinterprets (hooks) the first-order effect @e@ in the list using the provided natural transformation style handler.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interpose
    :: forall e ef eh
     . (e <| ef)
    => (e ~> Eff eh ef)
    -- ^ Effect handler
    -> Eff eh ef ~> Eff eh ef
interpose f = interposeRecWith (stateless f)
{-# INLINE interpose #-}

{- | Reinterprets (hooks) the first-order effect @e@ in the list using the provided continuational stateful handler.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interposeWith
    :: forall e ef a
     . (e <| ef)
    => Interpreter e (Eff '[] ef) a
    -- ^ Effect handler
    -> Eff '[] ef a
    -> Eff '[] ef a
interposeWith = interposeBy pure
{-# INLINE interposeWith #-}

{- | Reinterprets (hooks) the first-order effect @e@ in the list using the provided value handler and continuational stateful handler.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interposeBy
    :: forall e ef eh ans a
     . (e <| ef)
    => (a -> Eff eh ef ans)
    -- ^ Value handler
    -> Interpreter e (Eff eh ef) ans
    -- ^ Effect handler
    -> Eff '[] ef a
    -> Eff eh ef ans
interposeBy ret f = iterAllEffHFBy ret nilH \u -> maybe (`sendUnionBy` u) f (prj @e u)
{-# INLINE interposeBy #-}

{- | Reinterprets (hooks) the first-order effect @e@ in the list using the provided continuational stateful handler.

Interpretation is performed recursively with respect to the scopes of unelaborated higher-order effects @eh@.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond scopes.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interposeRecWith
    :: forall e ef eh a
     . (e <| ef)
    => (forall ans. Interpreter e (Eff eh ef) ans)
    -- ^ Effect handler
    -> Eff eh ef a
    -> Eff eh ef a
interposeRecWith f = loop
  where
    loop :: Eff eh ef ~> Eff eh ef
    loop = iterAllEffHFBy
        pure
        (flip sendUnionHBy . hfmapUnion loop)
        \u -> maybe (`sendUnionBy` u) f (prj @e u)
{-# INLINE interposeRecWith #-}

-- ** For higher-order effects

{- | Reinterprets (hooks) the higher-order effect @e@ in the list using the provided natural transformation style elaborator.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interposeH
    :: forall e eh ef
     . (e <<| eh, HFunctor e)
    => e ~~> Eff eh ef
    -- ^ Effect elaborator
    -> Eff eh ef ~> Eff eh ef
interposeH f = interposeRecHWith (stateless f)
{-# INLINE interposeH #-}

{- | Reinterprets (hooks) the higher-order effect @e@ in the list using the provided continuational stateful elaborator.

Interpretation is performed recursively with respect to the scopes of unelaborated higher-order effects @eh@.
Note that during interpretation, the continuational state is reset (delimited) and does not persist beyond scopes.

If multiple instances of @e@ exist in the list, the one closest to the head (with the smallest index) will be targeted.
-}
interposeRecHWith
    :: forall e eh ef a
     . (e <<| eh, HFunctor e)
    => (forall ans. Elaborator e (Eff eh ef) ans)
    -- ^ Effect elaborator
    -> Eff eh ef a
    -> Eff eh ef a
interposeRecHWith f = loop
  where
    loop :: Eff eh ef ~> Eff eh ef
    loop =
        iterAllEffHFBy
            pure
            (hfmapUnion loop >>> \u -> maybe (`sendUnionHBy` u) f (prjH @e u))
            (flip sendUnionBy)
{-# INLINE interposeRecHWith #-}

-- * Transformation to monads

-- | Traverses a computation containing only a single first-order effect @e@ using the provided value handler and continuational stateful handler, transforming it into a monad @m@.
iterEffBy
    :: forall e m ans a
     . (Monad m)
    => (a -> m ans)
    -- ^ Value handler
    -> Interpreter e m ans
    -- ^ Effect handler
    -> Eff '[] '[e] a
    -> m ans
iterEffBy ret hdl = iterAllEffHFBy ret nilH (hdl . extract)
{-# INLINE iterEffBy #-}

-- | Traverses a computation containing only a single higher-order effect @e@ using the provided value handler and continuational stateful elaborator, transforming it into a monad @m@.
iterEffHBy
    :: forall e m ans a
     . (Monad m, HFunctor e)
    => (a -> m ans)
    -- ^ Value handler
    -> Interpreter (e (Eff '[e] '[])) m ans
    -- ^ Effect handler
    -> Eff '[e] '[] a
    -> m ans
iterEffHBy ret elb = iterAllEffHFBy ret (elb . extractH) nil
{-# INLINE iterEffHBy #-}

{- | Traverses a computation containing only a single higher-order effect @e@ using the provided natural transformation elaborator, transforming it into a monad @m@.

Traversal is performed recursively with respect to the scope of the higher-order effect @e@.
Note that during traversal, the continuational state is reset (delimited) and does not persist beyond scopes.
-}
iterEffRecH
    :: forall e m
     . (Monad m, HFunctor e)
    => e ~~> m
    -- ^ Effect elaborator
    -> Eff '[e] '[] ~> m
iterEffRecH elb = iterEffRecHWith $ stateless elb
{-# INLINE iterEffRecH #-}

{- | Traverses a computation containing only a single higher-order effect @e@ using the provided continuational stateful elaborator, transforming it into a monad @m@.

Traversal is performed recursively with respect to the scope of the higher-order effect @e@.
Note that during traversal, the continuational state is reset (delimited) and does not persist beyond scopes.
-}
iterEffRecHWith
    :: forall e m
     . (Monad m, HFunctor e)
    => (forall ans. Elaborator e m ans)
    -- ^ Effect elaborator
    -> Eff '[e] '[] ~> m
iterEffRecHWith elb = loop
  where
    loop :: Eff '[e] '[] ~> m
    loop = iterEffHBy pure (elb . hfmap loop)
{-# INLINE iterEffRecHWith #-}

{- | Traverses a computation containing only higher-order effects @eh@ and first-order effects @ef@ using the provided continuational stateful elaborator, transforming it into a monad @m@.

Traversal is performed recursively with respect to the scopes of higher-order effects.
Note that during traversal, the continuational state is reset (delimited) and does not persist beyond scopes.
-}
iterEffRecHFWith
    :: forall eh ef m
     . (Monad m, HFunctor eh)
    => (forall ans. Elaborator eh m ans)
    -- ^ Effect elaborator
    -> (forall ans. Interpreter ef m ans)
    -- ^ Effect handler
    -> Eff '[eh] '[ef] ~> m
iterEffRecHFWith fh ff = loop
  where
    loop :: Eff '[eh] '[ef] ~> m
    loop = iterEffHFBy pure (fh . hfmap loop) ff
{-# INLINE iterEffRecHFWith #-}

{- | Traverses a computation containing only higher-order effects @eh@ and first-order effects @ef@ using the provided value handler,
continuational stateful elaborator, and handler, transforming it into a monad @m@.
-}
iterEffHFBy
    :: forall eh ef m ans a
     . (Monad m, HFunctor eh)
    => (a -> m ans)
    -- ^ Value handler
    -> Interpreter (eh (Eff '[eh] '[ef])) m ans
    -- ^ Effect elaborator
    -> Interpreter ef m ans
    -- ^ Effect handler
    -> Eff '[eh] '[ef] a
    -> m ans
iterEffHFBy ret elb hdl = iterAllEffHFBy ret (elb . extractH) (hdl . extract)
{-# INLINE iterEffHFBy #-}

-- | Traverses all effects using the provided value handler, continuational stateful elaborator, and handler, transforming them into a monad @m@.
iterAllEffHFBy
    :: forall eh ef m ans a
     . (Monad m)
    => (a -> m ans)
    -- ^ Value handler
    -> Interpreter (UnionH eh (Eff eh ef)) m ans
    -- ^ Effect elaborator
    -> Interpreter (Union ef) m ans
    -- ^ Effect handler
    -> Eff eh ef a
    -> m ans
iterAllEffHFBy ret fh ff = loop
  where
    loop = \case
        Val x -> ret x
        Op u q -> either fh ff u k
          where
            k = loop . qApp q
{-# INLINE iterAllEffHFBy #-}

-- | Traverses all effects using the provided natural-transformation style elaborator and handler, transforming them into a monad @m@.
iterAllEffHF
    :: forall eh ef m
     . (Monad m)
    => UnionH eh ~~> m
    -- ^ Effect elaborator
    -> Union ef ~> m
    -- ^ Effect handler
    -> Eff eh ef ~> m
iterAllEffHF fh ff = loop
  where
    loop :: Eff eh ef ~> m
    loop = \case
        Val x -> pure x
        Op u q -> either (fh . hfmapUnion loop >=> k) (ff >=> k) u
          where
            k = loop . qApp q
{-# INLINE iterAllEffHF #-}

-- * Layer manipulation

splitLayer :: Eff '[] ef ~> Eff eh '[Eff '[] ef]
splitLayer =
    iterAllEffHFBy pure nilH (\u k -> send0 (sendUnion u) >>= k)

mergeLayer :: Eff eh '[Eff eh ef] ~> Eff eh ef
mergeLayer =
    iterAllEffHFBy
        pure
        (\u k -> sendUnionH (hfmapUnion mergeLayer u) >>= k)
        (\u k -> extract u >>= k)

-- * Utilities

-- | Lifts a natural transformation into a continuational stateful interpreter.
stateless :: forall e m ans. (Monad m) => (e ~> m) -> Interpreter e m ans
stateless i e k = i e >>= k
{-# INLINE stateless #-}

-- | Applies a value to a Kleisli arrow in 'FTCQueue' representation.
qApp :: FTCQueue (Eff eh ef) a b -> a -> Eff eh ef b
qApp q' x = case tviewl q' of
    TOne k -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        Op u q -> Op u (q >< t)

interleave :: Eff eh ef a -> Eff eh ef b -> Eff eh ef (a, b)
interleave (Val x) m = (x,) <$> m
interleave m (Val x) = m <&> (,x)
interleave (Op u k) (Op u' k') = do
    x <- Op u (tsingleton pure)
    y <- Op u' (tsingleton pure)
    interleave (qApp k x) (qApp k' y)
