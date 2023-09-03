{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia where

import Control.Applicative (Alternative)
import Control.Effect.Class (
    LiftIns (LiftIns),
    SendIns,
    SendSig,
    SendVia (SendVia),
    Signature,
    runSendVia,
    sendIns,
    sendSig,
    unliftIns,
    type (~>),
 )
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Freer (Freer, liftIns)
import Control.Monad (MonadPlus)
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
import GHC.Generics qualified as F

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

type HeftiaEffects h u es = SendVia (HeftiaUnion h u es)

runHeftiaEffects :: HeftiaEffects h u es ~> h (u es)
runHeftiaEffects = runHeftiaUnion . runSendVia

heftiaEffects :: h (u es) ~> HeftiaEffects h u es
heftiaEffects = SendVia . HeftiaUnion

instance
    (Heftia c h, Union u, Member u (LiftIns e) es, HFunctor (u es)) =>
    SendIns e (HeftiaUnion h u es)
    where
    sendIns = HeftiaUnion . liftSig . inject . LiftIns

instance
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    SendSig e (HeftiaUnion h u es)
    where
    sendSig = HeftiaUnion . liftSig . hfmap runHeftiaUnion . inject

interpret ::
    (Heftia c h, Union u, HFunctor (u es), HFunctor (u (e : es)), HFunctor e) =>
    (e (HeftiaEffects h u es) ~> HeftiaEffects h u es) ->
    HeftiaEffects h u (e ': es) ~> HeftiaEffects h u es
interpret i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ interpretH \u ->
        case decomp u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSig e

reinterpret ::
    (Heftia c h, Union u, HFunctor (u (e : es)), HFunctor e) =>
    (e (HeftiaEffects h u (e ': es)) ~> HeftiaEffects h u (e ': es)) ->
    HeftiaEffects h u (e ': es) ~> HeftiaEffects h u (e ': es)
reinterpret i a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reinterpretH \u ->
        case decomp u of
            Left e -> runHeftiaEffects $ i $ hfmap heftiaEffects e
            Right e -> liftSig $ weakenR e

translate ::
    ( Heftia c h
    , Union u
    , HFunctor (u (e : es))
    , HFunctor (u (e' : es))
    , HFunctor e
    , HFunctor e'
    ) =>
    (e (HeftiaEffects h u (e' ': es)) ~> e' (HeftiaEffects h u (e' ': es))) ->
    HeftiaEffects h u (e ': es) ~> HeftiaEffects h u (e' ': es)
translate f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateH \u ->
        case decomp u of
            Left e -> weakenL $ hfmap runHeftiaEffects $ f $ hfmap heftiaEffects e
            Right e -> weakenR e

interpose ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es)) =>
    (e (HeftiaEffects h u es) ~> HeftiaEffects h u es) ->
    HeftiaEffects h u es ~> HeftiaEffects h u es
interpose f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ reinterpretH \u ->
        let u' = hfmap (interpose f . heftiaEffects) u
         in case project @_ @e u' of
                Just e -> runHeftiaEffects $ f e
                Nothing -> liftSig $ hfmap runHeftiaEffects u'

intercept ::
    forall e h u es c.
    (Heftia c h, Union u, Member u e es, HFunctor (u es), HFunctor e) =>
    (e (HeftiaEffects h u es) ~> e (HeftiaEffects h u es)) ->
    HeftiaEffects h u es ~> HeftiaEffects h u es
intercept f a =
    heftiaEffects $ ($ runHeftiaEffects a) $ translateH \u ->
        let u' = hfmap (intercept f . heftiaEffects) u
         in case project @_ @e u' of
                Just e -> inject $ hfmap runHeftiaEffects $ f e
                Nothing -> hfmap runHeftiaEffects u'

raiseUnder ::
    forall e' e es h u c.
    (Heftia c h, HFunctor (u (e : es)), HFunctor (u (e : e' : es)), Union u) =>
    HeftiaEffects h u (e ': es) ~> HeftiaEffects h u (e ': e' ': es)
raiseUnder a =
    heftiaEffects
        . ($ runHeftiaEffects a)
        $ translateH \u -> case decomp u of
            Left e -> weakenL e
            Right e -> weakenR $ weakenR e

heftiaToFreer ::
    (Heftia c h, Freer c' f, c (f (i F.:+: h e))) =>
    h (LiftIns (i F.:+: h e)) ~> f (i F.:+: h e)
heftiaToFreer a = ($ a) $ interpretH \case
    LiftIns (F.L1 i) -> liftIns $ F.L1 i
    LiftIns (F.R1 h) -> liftIns $ F.R1 h

{-
interpretT ::
    (Heftia c h, c (t (HeftiaEffects h u es)), HFunctor (u (LiftIns i : es)), Union u) =>
    (i ~> t (HeftiaEffects h u es)) ->
    HeftiaEffects h u (LiftIns i ': es) a ->
    t (HeftiaEffects h u es) a
interpretT i a =
    ($ runHeftiaEffects a) $ interpretH \u ->
        case decomp u of
            Left (LiftIns e) -> i e
            Right a -> undefined

splitIns ::
    ( Heftia c h
    , HFunctor (u (LiftIns i : es))
    , c (h (LiftIns (i F.:+: h (u es))))
    , Union u
    , HFunctor (u es)
    ) =>
    h (u (LiftIns i ': es)) ~> h (LiftIns (i F.:+: h (u es)))
splitIns = interpretH \u -> case decomp u of
    Left (LiftIns i) -> liftSig $ LiftIns $ F.L1 i
    Right u -> liftSig $ LiftIns $ F.R1 $ liftSig u'
      where
        u' = hfmap undefined u

heftiaToFreer ::
    (Heftia c h, Freer c' f, c (f (i F.:+: h e))) =>
    h (LiftIns (i F.:+: h e)) ~> f (i F.:+: h e)
heftiaToFreer a = ($ a) $ interpretH \case
    LiftIns (F.L1 i) -> liftIns $ F.L1 i
    LiftIns (F.R1 h) -> liftIns $ F.R1 h

interpretT ::
    forall e t h u es a c.
    ( Heftia c h
    , HFunctor (u (e : es))
    , HFunctor (u (e : LiftIns (t (HeftiaEffects h u es)) : es))
    , Union u
    , HFunctor e
    , HFunctor (u (LiftIns (t (HeftiaEffects h u es)) : es))
    , c (t (HeftiaEffects h u es))
    , MonadTransControl t
    , Monad (h (u es))
    , Monad (t (HeftiaEffects h u es))
    ) =>
    (e (t (HeftiaEffects h u es)) ~> t (HeftiaEffects h u es)) ->
    HeftiaEffects h u (e ': es) a ->
    t (HeftiaEffects h u es) a
interpretT i a = runBase a''
  where
    a'' = ($ a') $ interpret $ heftiaEffects . liftSig . weakenL . LiftIns . i . hfmap runBase
    a' = raiseUnder @(LiftIns (t (HeftiaEffects h u es))) a

runBase ::
    ( Heftia c h
    , HFunctor (u (LiftIns (t (HeftiaEffects h u es)) : es))
    , c (t (HeftiaEffects h u es))
    , Union u
    , MonadTransControl t
    , Monad (h (u es))
    , Monad (t (HeftiaEffects h u es))
    ) =>
    HeftiaEffects h u (LiftIns (t (HeftiaEffects h u es)) : es)
        ~> t (HeftiaEffects h u es)
runBase =
    runHeftiaEffects >>> interpretH \u -> case decomp u of
        Left (LiftIns a) -> a
        Right e -> controlT \run ->
            let a' = undefined e
             in heftiaEffects undefined

runBaseK ::
    ( Heftia c h
    , HFunctor (u (LiftIns (ContT r (HeftiaEffects h u es)) : es))
    , c (ContT r (HeftiaEffects h u es))
    , Union u
    , Monad (HeftiaEffects h u es)
    , HFunctor (u es)
    ) =>
    HeftiaEffects h u (LiftIns (ContT r (HeftiaEffects h u es)) : es)
        ~> ContT r (HeftiaEffects h u es)
runBaseK =
    runHeftiaEffects >>> interpretH \u -> case decomp u of
        Left (LiftIns a) -> a
        Right e -> ContT \k -> do
            let e' = hfmap (\m -> runContT undefined undefined) e
            x <- undefined
            k x
-}
