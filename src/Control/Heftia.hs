{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia where

import Control.Effect.Class (LiftIns, unliftIns)
import Control.Effect.Class.HFunctor (HFunctor, hmap)
import Control.Natural (type (~>))
import Data.Hefty.Union (Member, Union, decomp, inject, project, weakenL, weakenR, weakenSig, type (<:))

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

send :: (Heftia c h, Union u, Member u e es, HFunctor (u es)) => e (h (u es)) ~> h (u es)
send = liftSig . inject

interpret ::
    (Heftia c h, Union u, HFunctor (u es), HFunctor (u (e : es))) =>
    (e (h (u es)) ~> h (u es)) ->
    h (u (e ': es)) ~> h (u es)
interpret i =
    interpretH \u ->
        case decomp u of
            Left e -> i e
            Right e -> liftSig e

reinterpret ::
    (Heftia c h, Union u, HFunctor (u (e : es))) =>
    (e (h (u (e ': es))) ~> h (u (e ': es))) ->
    h (u (e ': es)) ~> h (u (e ': es))
reinterpret i =
    reinterpretH \u ->
        case decomp u of
            Left e -> i e
            Right e -> liftSig $ weakenR e

translate ::
    (Heftia c h, Union u, HFunctor (u (e : es)), HFunctor (u (e' : es))) =>
    (e (h (u (e' ': es))) ~> e' (h (u (e' ': es)))) ->
    h (u (e ': es)) ~> h (u (e' ': es))
translate f =
    translateH \u ->
        case decomp u of
            Left e -> weakenL $ f e
            Right e -> weakenR e

interpose ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    (e (h (u es)) ~> h (u es)) ->
    h (u es) ~> h (u es)
interpose f = reinterpretH \u ->
    let u' = hmap (interpose f) u
     in case project @_ @e u' of
            Just e -> f e
            Nothing -> liftSig u'

intercept ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    (e (h (u es)) ~> e (h (u es))) ->
    h (u es) ~> h (u es)
intercept f = translateH \u ->
    let u' = hmap (intercept f) u
     in case project @_ @e u' of
            Just e -> inject $ f e
            Nothing -> u'

retract :: (Heftia c h, c m) => h (LiftIns m) a -> m a
retract = interpretH unliftIns

sendH :: (s <: t, Heftia c h, HFunctor s, HFunctor t) => s (h s) a -> h t a
sendH = liftSig . weakenSig . hmap (translateH weakenSig)
