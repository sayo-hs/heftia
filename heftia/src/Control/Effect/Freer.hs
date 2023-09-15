{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Freer where

import Control.Applicative (Alternative)
import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    Instruction,
    SendIns,
    Tag,
    getTag,
    runEffectsVia,
    sendIns,
    type (~>),
 )
import Control.Freer.Trans (
    TransFreer,
    hoistFreer,
    interposeLowerT,
    interpretFT,
    liftInsT,
    liftLowerFT,
    reinterpretFT,
    runInterpretF,
    transformT,
 )
import Control.Monad (MonadPlus)
import Control.Monad.Cont (ContT (ContT), runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Freer (MonadTransFreer, interpretMK, interpretMT, reinterpretMK, reinterpretMT)
import Control.Monad.Trans.Freer.Church (FreerChurchT)
import Data.Coerce (Coercible, coerce)
import Data.Free.Sum (SumUnion, caseF, pattern L1, pattern R1, type (+))
import Data.Free.Union (
    IsMember,
    Member,
    Union (
        absurdUnion,
        bundleUnion2,
        bundleUnion3,
        bundleUnion4,
        decomp,
        flipUnion,
        flipUnion3,
        flipUnionUnder,
        inject,
        inject0,
        project,
        rot3,
        rot3',
        unbundleUnion2,
        unbundleUnion3,
        unbundleUnion4,
        weaken,
        weaken2,
        weaken2Under,
        weaken2Under2,
        weaken3,
        weaken3Under,
        weaken4,
        weakenUnder,
        weakenUnder2,
        weakenUnder3
    ),
    (|+|:),
 )
import Data.Function ((&))
import Data.Kind (Type)

newtype
    FreerUnion
        (fr :: Instruction -> (Type -> Type) -> Type -> Type)
        u
        (es :: [Instruction])
        f
        a = FreerUnion {runFreerUnion :: fr (u es) f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

type FreerEffects fr u es f = EffectsVia EffectDataHandler (FreerUnion fr u es f)

unFreerEffects :: FreerEffects fr u es f ~> fr (u es) f
unFreerEffects = runFreerUnion . runEffectsVia
{-# INLINE unFreerEffects #-}

freerEffects :: fr (u es) f ~> FreerEffects fr u es f
freerEffects = EffectsVia . FreerUnion
{-# INLINE freerEffects #-}

newtype FreerUnionForSend handleHere fr u es f a = FreerUnionForSend
    {runFreerUnionForSend :: FreerUnion fr u es f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

instance
    SendIns e (FreerUnionForSend (e `IsMember` es) fr u es f) =>
    SendIns e (FreerUnion fr u es f)
    where
    sendIns = runFreerUnionForSend @(e `IsMember` es) . sendIns
    {-# INLINE sendIns #-}

instance
    (TransFreer c fr, Union u, Member u e es) =>
    SendIns e (FreerUnionForSend 'True fr u es f)
    where
    sendIns = FreerUnionForSend . FreerUnion . liftInsT . inject
    {-# INLINE sendIns #-}

instance (TransFreer c fr, SendIns e f, c f) => SendIns e (FreerUnionForSend 'False fr u es f) where
    sendIns = FreerUnionForSend . FreerUnion . liftLowerFT . sendIns
    {-# INLINE sendIns #-}

interpret ::
    (TransFreer c fr, Union u, c f) =>
    (e ~> FreerEffects fr u es f) ->
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u es f
interpret i =
    overFreerEffects $ interpretFT liftLowerFT \u ->
        case decomp u of
            Left e -> unFreerEffects $ i e
            Right e -> liftInsT e

interpretT ::
    forall t fr u e es f.
    (MonadTransFreer fr, Union u, MonadTrans t, Monad f, Monad (t (FreerEffects fr u es f))) =>
    (e ~> t (FreerEffects fr u es f)) ->
    FreerEffects fr u (e ': es) f ~> t (FreerEffects fr u es f)
interpretT i = interpretMT i . splitFreerEffects @_ @fr
{-# INLINE interpretT #-}

interpretK ::
    (MonadTransFreer fr, Union u, Monad f) =>
    (a -> FreerEffects fr u es f r) ->
    (forall x. (x -> FreerEffects fr u es f r) -> e x -> FreerEffects fr u es f r) ->
    FreerEffects fr u (e ': es) f a ->
    FreerEffects fr u es f r
interpretK k i = (`runContT` k) . interpretContT \e -> ContT (`i` e)
{-# INLINE interpretK #-}

interpretContT ::
    forall r fr u e es f.
    (MonadTransFreer fr, Union u, Monad f) =>
    (e ~> ContT r (FreerEffects fr u es f)) ->
    FreerEffects fr u (e ': es) f ~> ContT r (FreerEffects fr u es f)
interpretContT i = interpretMK i . splitFreerEffects @_ @fr
{-# INLINE interpretContT #-}

interpretAll ::
    (TransFreer c fr, Union u, c f, c g) =>
    (f ~> g) ->
    (u es ~> g) ->
    (e ~> g) ->
    FreerEffects fr u (e ': es) f ~> g
interpretAll iLower iOther iTarget a =
    unFreerEffects a & interpretFT iLower \u ->
        case decomp u of
            Left e -> iTarget e
            Right e -> iOther e

reinterpret ::
    (TransFreer c fr, Union u, c f) =>
    (e ~> FreerEffects fr u (e ': es) f) ->
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u (e ': es) f
reinterpret i =
    overFreerEffects $ reinterpretFT \u ->
        case decomp u of
            Left e -> unFreerEffects $ i e
            Right e -> liftInsT $ weaken e

transformAll ::
    (TransFreer c fr, Union u, Union u', c f) =>
    (u es ~> u' es') ->
    FreerEffects fr u es f ~> FreerEffects fr u' es' f
transformAll f = overFreerEffects $ transformT f
{-# INLINE transformAll #-}

transform ::
    forall e' e fr u r f c.
    (TransFreer c fr, Union u, c f) =>
    (e ~> e') ->
    FreerEffects fr u (e ': r) f ~> FreerEffects fr u (e' ': r) f
transform f =
    overFreerEffects $ transformT \u ->
        case decomp u of
            Left e -> inject0 $ f e
            Right e -> weaken e

untag ::
    forall tag e fr u r f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (Tag e tag ': r) f ~> FreerEffects fr u (e ': r) f
untag = transform getTag

interpose ::
    forall e fr u es f c.
    (TransFreer c fr, Union u, Member u e es, c f) =>
    (e ~> FreerEffects fr u es f) ->
    FreerEffects fr u es f ~> FreerEffects fr u es f
interpose f =
    overFreerEffects $ reinterpretFT \u ->
        case project @_ @e u of
            Just e -> unFreerEffects $ f e
            Nothing -> liftInsT u

interposeT ::
    forall e t fr u es m.
    ( MonadTransFreer fr
    , Union u
    , Member u e es
    , Monad m
    , MonadTrans t
    , forall m1 m2 x. Coercible m1 m2 => Coercible (t m1 x) (t m2 x)
    , Monad (t (fr (u es) m))
    ) =>
    (e ~> t (FreerEffects fr u es m)) ->
    FreerEffects fr u es m ~> t (FreerEffects fr u es m)
interposeT f a =
    hoistT @(fr (u es) m) $
        unFreerEffects a & reinterpretMT \u ->
            case project @_ @e u of
                Just e -> hoistT $ f e
                Nothing -> lift $ liftInsT u
  where
    hoistT :: Coercible (t m1 a) (t m2 a) => t m1 a -> t m2 a
    hoistT = coerce
    {-# INLINE hoistT #-}

interposeAll ::
    forall e g fr u es f c.
    ( TransFreer c fr
    , Union u
    , Member u e es
    , c f
    , c g
    ) =>
    (f ~> g) ->
    (u es ~> g) ->
    (e ~> g) ->
    FreerEffects fr u es f ~> g
interposeAll iLower iOther iTarget a =
    unFreerEffects a & interpretFT iLower \u ->
        case project @_ @e u of
            Just e -> iTarget e
            Nothing -> iOther u

interposeK ::
    (MonadTransFreer fr, Union u, Member u e es, Monad m) =>
    (a -> FreerEffects fr u es m r) ->
    (forall x. (x -> FreerEffects fr u es m r) -> e x -> FreerEffects fr u es m r) ->
    FreerEffects fr u es m a ->
    FreerEffects fr u es m r
interposeK k i = (`runContT` k) . interposeContT \e -> ContT (`i` e)
{-# INLINE interposeK #-}

interposeContT ::
    forall e r fr u es m.
    (MonadTransFreer fr, Union u, Member u e es, Monad m) =>
    (e ~> ContT r (FreerEffects fr u es m)) ->
    FreerEffects fr u es m ~> ContT r (FreerEffects fr u es m)
interposeContT f a =
    hoistContT $
        unFreerEffects a & reinterpretMK \u ->
            case project @_ @e u of
                Just e -> hoistContT $ f e
                Nothing -> lift $ liftInsT u
  where
    hoistContT :: Coercible m1 m2 => ContT r m1 a -> ContT r m2 a
    hoistContT = coerce
    {-# INLINE hoistContT #-}

intercept ::
    forall e fr u es f c.
    (TransFreer c fr, Union u, Member u e es, c f) =>
    (e ~> e) ->
    FreerEffects fr u es f ~> FreerEffects fr u es f
intercept f =
    overFreerEffects $ transformT \u ->
        case project @_ @e u of
            Just e -> inject $ f e
            Nothing -> u

raise ::
    forall e es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e ': es) f
raise = transformAll weaken
{-# INLINE raise #-}

raise2 ::
    forall e1 e2 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': es) f
raise2 = transformAll weaken2
{-# INLINE raise2 #-}

raise3 ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raise3 = transformAll weaken3
{-# INLINE raise3 #-}

raise4 ::
    forall e1 e2 e3 e4 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise4 = transformAll weaken4
{-# INLINE raise4 #-}

raiseUnder ::
    forall e1 e2 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': es) f
raiseUnder = transformAll weakenUnder
{-# INLINE raiseUnder #-}

raiseUnder2 ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raiseUnder2 = transformAll weakenUnder2
{-# INLINE raiseUnder2 #-}

raiseUnder3 ::
    forall e1 e2 e3 e4 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raiseUnder3 = transformAll weakenUnder3
{-# INLINE raiseUnder3 #-}

raise2Under ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raise2Under = transformAll weaken2Under
{-# INLINE raise2Under #-}

raise2Under2 ::
    forall e1 e2 e3 e4 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise2Under2 = transformAll weaken2Under2
{-# INLINE raise2Under2 #-}

raise3Under ::
    forall e1 e2 e3 e4 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise3Under = transformAll weaken3Under
{-# INLINE raise3Under #-}

flipFreer ::
    forall e1 e2 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e2 ': e1 ': es) f
flipFreer = transformAll flipUnion
{-# INLINE flipFreer #-}

flipFreer3 ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e3 ': e2 ': e1 ': es) f
flipFreer3 = transformAll flipUnion3
{-# INLINE flipFreer3 #-}

flipFreerUnder ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e1 ': e3 ': e2 ': es) f
flipFreerUnder = transformAll flipUnionUnder
{-# INLINE flipFreerUnder #-}

rotate3 ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e2 ': e3 ': e1 ': es) f
rotate3 = transformAll rot3
{-# INLINE rotate3 #-}

rotate3' ::
    forall e1 e2 e3 es fr u f c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e3 ': e1 ': e2 ': es) f
rotate3' = transformAll rot3'
{-# INLINE rotate3' #-}

bundle2 ::
    forall e1 e2 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (u' '[e1, e2] ': es) f
bundle2 = transformAll bundleUnion2
{-# INLINE bundle2 #-}

bundle3 ::
    forall e1 e2 e3 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (u' '[e1, e2, e3] ': es) f
bundle3 = transformAll bundleUnion3
{-# INLINE bundle3 #-}

bundle4 ::
    forall e1 e2 e3 e4 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f ~> FreerEffects fr u (u' '[e1, e2, e3, e4] ': es) f
bundle4 = transformAll bundleUnion4
{-# INLINE bundle4 #-}

unbundle2 ::
    forall e1 e2 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2] ': es) f ~> FreerEffects fr u (e1 ': e2 ': es) f
unbundle2 = transformAll unbundleUnion2
{-# INLINE unbundle2 #-}

unbundle3 ::
    forall e1 e2 e3 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2, e3] ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
unbundle3 = transformAll unbundleUnion3
{-# INLINE unbundle3 #-}

unbundle4 ::
    forall e1 e2 e3 e4 es fr u f c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2, e3, e4] ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
unbundle4 = transformAll unbundleUnion4
{-# INLINE unbundle4 #-}

hoistFreerEffects ::
    (TransFreer c fr, c f, c g) => (f ~> g) -> FreerEffects fr u es f ~> FreerEffects fr u es g
hoistFreerEffects f = overFreerEffects $ hoistFreer f
{-# INLINE hoistFreerEffects #-}

lowerToIns ::
    (TransFreer c fr, c g, c (f + g), Union u) =>
    FreerEffects fr u es (f + g) ~> FreerEffects fr u (f ': es) g
lowerToIns =
    overFreerEffects $
        interpretFT
            (caseF (liftInsT . inject0) liftLowerFT)
            (liftInsT . weaken)
{-# INLINE lowerToIns #-}

insToLower ::
    (TransFreer c fr, c (f + g), c g, Union u) =>
    FreerEffects fr u (f ': es) g ~> FreerEffects fr u es (f + g)
insToLower = overFreerEffects $ interpretFT (liftLowerFT . R1) (liftLowerFT . L1 |+|: liftInsT)
{-# INLINE insToLower #-}

interposeLower ::
    (TransFreer c fr, c f, c g) =>
    (f ~> FreerEffects fr u es g) ->
    FreerEffects fr u es f ~> FreerEffects fr u es g
interposeLower f = overFreerEffects $ interposeLowerT (unFreerEffects . f)
{-# INLINE interposeLower #-}

overFreerEffects ::
    (fr (u es) f a -> fr' (u' es') g b) ->
    FreerEffects fr u es f a ->
    FreerEffects fr' u' es' g b
overFreerEffects f = freerEffects . f . unFreerEffects
{-# INLINE overFreerEffects #-}

interpreted :: (TransFreer c fr, c f, Union u) => FreerEffects fr u '[] f ~> f
interpreted = runInterpretF absurdUnion . unFreerEffects
{-# INLINE interpreted #-}

splitFreerEffects ::
    (TransFreer c fr', TransFreer c fr, c f, c (FreerEffects fr u es f), Union u) =>
    FreerEffects fr u (e ': es) f ~> fr' e (FreerEffects fr u es f)
splitFreerEffects a =
    unFreerEffects a & interpretFT (liftLowerFT . freerEffects . liftLowerFT) \u ->
        case decomp u of
            Left e -> liftInsT e
            Right e -> liftLowerFT $ freerEffects $ liftInsT e

liftLower :: (TransFreer c fr, c f) => f ~> FreerEffects fr u es f
liftLower = freerEffects . liftLowerFT
{-# INLINE liftLower #-}

runIO :: MonadIO m => Fre (IO ': es) m ~> Fre es m
runIO = interpret $ liftLower . liftIO
{-# INLINE runIO #-}

runInterpret :: (TransFreer c fr, c f) => (u es ~> f) -> FreerEffects fr u es f ~> f
runInterpret f = runInterpretF f . unFreerEffects
{-# INLINE runInterpret #-}

runFreerEffects ::
    (TransFreer c fr, c f, Union u) =>
    FreerEffects fr u '[f] f ~> f
runFreerEffects = runInterpret $ id |+|: absurdUnion
{-# INLINE runFreerEffects #-}

type Fre es f = FreerEffects FreerChurchT SumUnion es f

-- type FreA es f = FreerEffects (FreerFinalT Applicative) SumUnion es f

type e <| es = Member SumUnion e es
