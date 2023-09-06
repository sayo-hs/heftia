{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Heftia where

import Control.Applicative (Alternative)
import Control.Arrow ((>>>))
import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    LiftIns (LiftIns),
    SendIns,
    SendSig,
    Signature,
    runEffectsVia,
    sendIns,
    sendSig,
    type (~>),
 )
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Effect.Freer (FreerEffects, freerEffects, interpose, runFreerEffects)
import Control.Freer.Trans (TransFreer, interpretFT, liftInsT, liftLower)
import Control.Heftia.Trans (
    TransHeftia,
    elaborateHT,
    hoistHeftia,
    interpretLowerH,
    liftLowerH,
    liftSigT,
    reelaborateHT,
    runElaborateH,
    translateT,
 )
import Control.Heftia.Trans.Final (HeftiaFinalT)
import Control.Monad (MonadPlus)
import Control.Monad.Cont (ContT (ContT), MonadTrans, runContT)
import Control.Monad.Trans.Heftia (MonadTransHeftia, elaborateMK, elaborateMT)
import Data.Free.Union (Member, Union, project)
import Data.Hefty.Sum (SumUnionH)
import Data.Hefty.Union (
    MemberH,
    UnionH,
    decompH,
    injectH,
    projectH,
    weakenLH,
    weakenRH,
 )
import Data.Kind (Type)

newtype
    HeftiaUnion
        (h :: Signature -> (Type -> Type) -> Type -> Type)
        u
        (es :: [Signature])
        f
        a = HeftiaUnion {runHeftiaUnion :: h (u es) f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

type HeftiaEffects h u es f = EffectsVia EffectDataHandler (HeftiaUnion h u es f)

runHeftiaEffects :: HeftiaEffects h u es f ~> h (u es) f
runHeftiaEffects = runHeftiaUnion . runEffectsVia
{-# INLINE runHeftiaEffects #-}

heftiaEffects :: h (u es) f ~> HeftiaEffects h u es f
heftiaEffects = EffectsVia . HeftiaUnion
{-# INLINE heftiaEffects #-}

instance
    (TransHeftia c h, UnionH u, MemberH u (LiftIns e) es, HFunctor (u es)) =>
    SendIns e (HeftiaUnion h u es f)
    where
    sendIns = HeftiaUnion . liftSigT . injectH . LiftIns
    {-# INLINE sendIns #-}

instance
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es)) =>
    SendSig e (HeftiaUnion h u es f)
    where
    sendSig = HeftiaUnion . liftSigT . hfmap runHeftiaUnion . injectH
    {-# INLINE sendSig #-}

runElaborate ::
    (TransHeftia c h, HFunctor (u es), c f, UnionH u) =>
    (u es f ~> f) ->
    HeftiaEffects h u es f ~> f
runElaborate f = runElaborateH f . runHeftiaEffects
{-# INLINE runElaborate #-}

runElaborateK ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, Monad m) =>
    (a -> m r) ->
    (forall x. (x -> m r) -> u es (ContT r m) x -> m r) ->
    HeftiaEffects h u es m a ->
    m r
runElaborateK k f = (`runContT` k) . runElaborateContT \e -> ContT (`f` e)
{-# INLINE runElaborateK #-}

runElaborateContT ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, Monad m) =>
    (u es (ContT r m) ~> ContT r m) ->
    HeftiaEffects h u es m ~> ContT r m
runElaborateContT f = elaborateMK f . runHeftiaEffects
{-# INLINE runElaborateContT #-}

runElaborateT ::
    (MonadTransHeftia h, HFunctor (u es), UnionH u, MonadTrans t, Monad m, Monad (t m)) =>
    (u es (t m) ~> t m) ->
    HeftiaEffects h u es m ~> t m
runElaborateT f = elaborateMT f . runHeftiaEffects
{-# INLINE runElaborateT #-}

elaborate ::
    (TransHeftia c h, HFunctor (u es), c f, UnionH u, c g) =>
    (f ~> g) ->
    (u es g ~> g) ->
    HeftiaEffects h u es f ~> g
elaborate f g = elaborateHT f g . runHeftiaEffects
{-# INLINE elaborate #-}

interpretH ::
    (TransHeftia c h, UnionH u, HFunctor (u es), HFunctor (u (e : es)), HFunctor e, c f) =>
    (e (HeftiaEffects h u es f) ~> HeftiaEffects h u es f) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u es f
interpretH i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ elaborateHT liftLowerH \u ->
        case decompH u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSigT e

reinterpretH ::
    (TransHeftia c h, UnionH u, HFunctor (u (e : es)), HFunctor e, c f) =>
    (e (HeftiaEffects h u (e ': es) f) ~> HeftiaEffects h u (e ': es) f) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u (e ': es) f
reinterpretH i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reelaborateHT \u ->
        case decompH u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSigT $ weakenRH e

translate ::
    ( TransHeftia c h
    , UnionH u
    , HFunctor (u (e : es))
    , HFunctor (u (e' : es))
    , HFunctor e
    , HFunctor e'
    , c f
    ) =>
    (e (HeftiaEffects h u (e' ': es) f) ~> e' (HeftiaEffects h u (e' ': es) f)) ->
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u (e' ': es) f
translate f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateT \u ->
        case decompH u of
            Left e -> weakenLH $ hfmap runHeftiaEffects $ f $ hfmap heftiaEffects e
            Right e -> weakenRH e

interposeH ::
    forall e h u es f c.
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es), c f) =>
    (e (HeftiaEffects h u es f) ~> HeftiaEffects h u es f) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es f
interposeH f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reelaborateHT \u ->
        let u' = hfmap (interposeH f . heftiaEffects) u
         in case projectH @_ @e u' of
                Just e -> runHeftiaEffects $ f e
                Nothing -> liftSigT $ hfmap runHeftiaEffects u'

interceptH ::
    forall e h u es f c.
    (TransHeftia c h, UnionH u, MemberH u e es, HFunctor (u es), HFunctor e, c f) =>
    (e (HeftiaEffects h u es f) ~> e (HeftiaEffects h u es f)) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es f
interceptH f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateT \u ->
        let u' = hfmap (interceptH f . heftiaEffects) u
         in case projectH @_ @e u' of
                Just e -> injectH $ hfmap runHeftiaEffects $ f e
                Nothing -> hfmap runHeftiaEffects u'

raiseUnderH ::
    forall e' e es h u f c.
    (TransHeftia c h, HFunctor (u (e : es)), HFunctor (u (e : e' : es)), UnionH u, c f) =>
    HeftiaEffects h u (e ': es) f ~> HeftiaEffects h u (e ': e' ': es) f
raiseUnderH a =
    heftiaEffects
        . ($ runHeftiaEffects a)
        $ translateT \u -> case decompH u of
            Left e -> weakenLH e
            Right e -> weakenRH $ weakenRH e

hoistHeftiaEffects ::
    (TransHeftia c h, HFunctor (u es), c f, c g) =>
    (f ~> g) ->
    HeftiaEffects h u es f ~> HeftiaEffects h u es g
hoistHeftiaEffects f = heftiaEffects . hoistHeftia f . runHeftiaEffects
{-# INLINE hoistHeftiaEffects #-}

hoistInterpose ::
    forall e h u es fr u' es' f c c'.
    ( TransHeftia c h
    , HFunctor (u es)
    , TransFreer c' fr
    , Union u'
    , Member u' e es'
    , c (FreerEffects fr u' es' f)
    , c' f
    ) =>
    (e ~> FreerEffects fr u' es' f) ->
    HeftiaEffects h u es (FreerEffects fr u' es' f)
        ~> HeftiaEffects h u es (FreerEffects fr u' es' f)
hoistInterpose f = hoistHeftiaEffects $ interpose f
{-# INLINE hoistInterpose #-}

interposeIns ::
    forall e h u es fr u' es' f c c'.
    ( TransHeftia c h
    , HFunctor (u es)
    , TransFreer c' fr
    , Union u'
    , Member u' e es'
    , c (FreerEffects fr u' es' f)
    , c' f
    , c' (h (u es) (FreerEffects fr u' es' f))
    ) =>
    (e ~> HeftiaEffects h u es (FreerEffects fr u' es' f)) ->
    HeftiaEffects h u es (FreerEffects fr u' es' f)
        ~> HeftiaEffects h u es (FreerEffects fr u' es' f)
interposeIns f a =
    heftiaEffects
        . ($ runHeftiaEffects a)
        $ interpretLowerH
        $ runFreerEffects
            >>> interpretFT
                (liftLowerH . freerEffects . liftLower)
                \u -> case project @_ @e u of
                    Just e -> runHeftiaEffects $ f e
                    Nothing -> liftLowerH $ freerEffects $ liftInsT u

type Hef es f = HeftiaEffects (HeftiaFinalT Monad) SumUnionH es f
type HefA es f = HeftiaEffects (HeftiaFinalT Applicative) SumUnionH es f
