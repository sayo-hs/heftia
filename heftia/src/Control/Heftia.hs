{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns, Send, Signature, send, unliftIns, type (~>))
import Control.Effect.Class.HFunctor (HFunctor, hfmap)
import Control.Heftia.Final (HeftiaFinal)
import Control.Monad (MonadPlus)
import Data.Hefty.Sum (SumUnion)
import Data.Hefty.Union (
    Member,
    Union,
    decomp,
    inject,
    project,
    weakenL,
    weakenR,
    weakenSig,
    type (<:),
 )
import Data.Kind (Type)

class (forall sig. HFunctor sig => c (h sig)) => Heftia c h | h -> c where
    {-# MINIMAL liftSig, interpretH #-}

    -- | Lift a /signature/ into a Heftia monad.
    liftSig :: HFunctor sig => sig (h sig) a -> h sig a

    interpretH :: (c m, HFunctor sig) => (sig m ~> m) -> h sig a -> m a

    -- | Translate /signature/s embedded in a Heftia monad.
    translateH ::
        (HFunctor sig, HFunctor sig') =>
        (sig (h sig') ~> sig' (h sig')) ->
        h sig a ->
        h sig' a
    translateH phi = interpretH $ liftSig . phi
    {-# INLINE translateH #-}

    reinterpretH :: HFunctor sig => (sig (h sig) ~> h sig) -> h sig a -> h sig a
    reinterpretH = interpretH
    {-# INLINE reinterpretH #-}

retract :: (Heftia c h, c m) => h (LiftIns m) a -> m a
retract = interpretH unliftIns

sendH :: (s <: t, Heftia c h, HFunctor s, HFunctor t) => s (h s) a -> h t a
sendH = liftSig . weakenSig . hfmap (translateH weakenSig)

newtype HeftiaUnion (h :: Signature -> Type -> Type) u (es :: [Signature]) a = HeftiaUnion
    {runHeftiaUnion :: h (u es) a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

type Hef = HeftiaUnion (HeftiaFinal Monad) SumUnion
type HefA = HeftiaUnion (HeftiaFinal Applicative) SumUnion

instance
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    Send e (HeftiaUnion h u es)
    where
    send = HeftiaUnion . liftSig . hfmap runHeftiaUnion . inject

interpret ::
    (Heftia c h, Union u, HFunctor (u es), HFunctor (u (e : es)), HFunctor e) =>
    (e (HeftiaUnion h u es) ~> HeftiaUnion h u es) ->
    HeftiaUnion h u (e ': es) ~> HeftiaUnion h u es
interpret i (HeftiaUnion a) =
    HeftiaUnion $ ($ a) $ interpretH \u ->
        case decomp u of
            Left e -> runHeftiaUnion $ i $ hfmap HeftiaUnion e
            Right e -> liftSig e

reinterpret ::
    (Heftia c h, Union u, HFunctor (u (e : es)), HFunctor e) =>
    (e (HeftiaUnion h u (e ': es)) ~> HeftiaUnion h u (e ': es)) ->
    HeftiaUnion h u (e ': es) ~> HeftiaUnion h u (e ': es)
reinterpret i (HeftiaUnion a) =
    HeftiaUnion $ ($ a) $ reinterpretH \u ->
        case decomp u of
            Left e -> runHeftiaUnion $ i $ hfmap HeftiaUnion e
            Right e -> liftSig $ weakenR e

translate ::
    ( Heftia c h
    , Union u
    , HFunctor (u (e : es))
    , HFunctor (u (e' : es))
    , HFunctor e
    , HFunctor e'
    ) =>
    (e (HeftiaUnion h u (e' ': es)) ~> e' (HeftiaUnion h u (e' ': es))) ->
    HeftiaUnion h u (e ': es) ~> HeftiaUnion h u (e' ': es)
translate f (HeftiaUnion a) =
    HeftiaUnion $ ($ a) $ translateH \u ->
        case decomp u of
            Left e -> weakenL $ hfmap runHeftiaUnion $ f $ hfmap HeftiaUnion e
            Right e -> weakenR e

interpose ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    (e (HeftiaUnion h u es) ~> HeftiaUnion h u es) ->
    HeftiaUnion h u es ~> HeftiaUnion h u es
interpose f (HeftiaUnion a) =
    HeftiaUnion $ ($ a) $ reinterpretH \u ->
        let u' = hfmap (interpose f . HeftiaUnion) u
         in case project @_ @e u' of
                Just e -> runHeftiaUnion $ f e
                Nothing -> liftSig $ hfmap runHeftiaUnion u'

intercept ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es), HFunctor e) =>
    (e (HeftiaUnion h u es) ~> e (HeftiaUnion h u es)) ->
    HeftiaUnion h u es ~> HeftiaUnion h u es
intercept f (HeftiaUnion a) =
    HeftiaUnion $ ($ a) $ translateH \u ->
        let u' = hfmap (intercept f . HeftiaUnion) u
         in case project @_ @e u' of
                Just e -> inject $ hfmap runHeftiaUnion $ f e
                Nothing -> hfmap runHeftiaUnion u'
