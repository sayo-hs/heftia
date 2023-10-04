{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Freer carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
module Control.Effect.Freer where

import Control.Applicative (Alternative)
import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    Embed,
    Instruction,
    SendIns,
    Tag,
    getTag,
    runEffectsVia,
    sendIns,
    unEmbed,
    type (<:),
    type (~>),
 )
import Control.Effect.Class.Fail (FailI (Fail))
import Control.Effect.Class.Machinery.DepParams (QueryDepParamsFor)
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
import Data.Free.Extensible (ExtensibleUnion)
import Data.Free.Sum (caseF, pattern L1, pattern R1, type (+))
import Data.Free.Union (
    FirstDepParams,
    IsMember,
    Member,
    MemberDep,
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

{- |
A data type that wraps Freer with any encoding to become an instance of 'SendIns' based on the
`liftInsT` from the `TransFreer` type class.
-}
newtype
    FreerUnion
        (fr :: Instruction -> (Type -> Type) -> Type -> Type)
        u
        (es :: [Instruction])
        f
        a = FreerUnion {runFreerUnion :: fr (u es) f a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)
    deriving stock (Foldable, Traversable)

instance
    (IO <: FreerEffects fr u es f, Monad (fr (u es) f)) =>
    MonadIO (FreerUnion fr u es f)
    where
    liftIO = runEffectsVia @EffectDataHandler . sendIns

instance
    (FailI <: FreerEffects fr u es f, Monad (fr (u es) f)) =>
    MonadFail (FreerUnion fr u es f)
    where
    fail = runEffectsVia @EffectDataHandler . sendIns . Fail

{- |
A Freer carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
type FreerEffects fr u es f = EffectsVia EffectDataHandler (FreerUnion fr u es f)

-- | Unwrap the `FreerEffects` wrapper.
unFreerEffects :: forall es f fr u. FreerEffects fr u es f ~> fr (u es) f
unFreerEffects = runFreerUnion . runEffectsVia
{-# INLINE unFreerEffects #-}

-- | Wrap with `FreerEffects`.
freerEffects :: forall es f fr u. fr (u es) f ~> FreerEffects fr u es f
freerEffects = EffectsVia . FreerUnion
{-# INLINE freerEffects #-}

{- |
A wrapper data type designed to induce instance resolution to delegate the search for effect classes
to a lower carrier @f@ even when there are no target effect classes in the effect class list @es@.

When a target effect class exists within @es@, @handleHere@ is induced to be @'True@; when it
doesn't exist, it's induced to be @'False@.
-}
newtype FreerUnionForSend handleHere fr u es f a = FreerUnionForSend
    {runFreerUnionForSend :: FreerUnion fr u es f a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        )
    deriving stock (Foldable, Traversable)

deriving newtype instance
    (IO <: FreerEffects fr u es f, Monad (fr (u es) f)) =>
    MonadIO (FreerUnionForSend handleHere fr u es f)

deriving newtype instance
    (FailI <: FreerEffects fr u es f, Monad (fr (u es) f)) =>
    MonadFail (FreerUnionForSend handleHere fr u es f)

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

type instance QueryDepParamsFor eci (FreerUnion fr u es f) = FirstDepParams eci es f

-- | Interpret the leading effect class in the effect class list.
interpret ::
    forall e es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    (e ~> FreerEffects fr u es f) ->
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u es f
interpret i =
    overFreerEffects $ interpretFT liftLowerFT \u ->
        case decomp u of
            Left e -> unFreerEffects $ i e
            Right e -> liftInsT e

-- | Interpret the leading effect class in the effect class list using a monad transformer.
interpretT ::
    forall t e es f fr u.
    (MonadTransFreer fr, Union u, MonadTrans t, Monad f, Monad (t (FreerEffects fr u es f))) =>
    (e ~> t (FreerEffects fr u es f)) ->
    FreerEffects fr u (e ': es) f ~> t (FreerEffects fr u es f)
interpretT i = interpretMT i . splitFreerEffects @_ @fr
{-# INLINE interpretT #-}

-- | Interpret the leading effect class in the effect class list using a delimited continuation.
interpretK ::
    forall e r a es f fr u.
    (MonadTransFreer fr, Union u, Monad f) =>
    (a -> FreerEffects fr u es f r) ->
    (forall x. (x -> FreerEffects fr u es f r) -> e x -> FreerEffects fr u es f r) ->
    FreerEffects fr u (e ': es) f a ->
    FreerEffects fr u es f r
interpretK k i = (`runContT` k) . interpretContT \e -> ContT (`i` e)
{-# INLINE interpretK #-}

{- |
Interpret the leading effect class in the effect class list using a continuation monad transformer.
-}
interpretContT ::
    forall e r es f fr u.
    (MonadTransFreer fr, Union u, Monad f) =>
    (e ~> ContT r (FreerEffects fr u es f)) ->
    FreerEffects fr u (e ': es) f ~> ContT r (FreerEffects fr u es f)
interpretContT i = interpretMK i . splitFreerEffects @_ @fr
{-# INLINE interpretContT #-}

{- |
Interpret not only the leading effect class but also all the remaining effect classes and the
underlying carrier simultaneously, transforming them into any carrier @g@.
-}
interpretAll ::
    forall e g f es fr u c.
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

-- | Reinterpret the leading effect class in the effect class list.
reinterpret ::
    forall e es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    (e ~> FreerEffects fr u (e ': es) f) ->
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u (e ': es) f
reinterpret i =
    overFreerEffects $ reinterpretFT \u ->
        case decomp u of
            Left e -> unFreerEffects $ i e
            Right e -> liftInsT $ weaken e

-- | Transform all effect classes in the effect class list into another union of effect classes.
transformAll ::
    forall u' es es' f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    (u es ~> u' es') ->
    FreerEffects fr u es f ~> FreerEffects fr u' es' f
transformAll f = overFreerEffects $ transformT f
{-# INLINE transformAll #-}

-- | Transform the leading effect class in the effect class list into another effect class.
transform ::
    forall e' e r f fr u c.
    (TransFreer c fr, Union u, c f) =>
    (e ~> e') ->
    FreerEffects fr u (e ': r) f ~> FreerEffects fr u (e' ': r) f
transform f =
    overFreerEffects $ transformT \u ->
        case decomp u of
            Left e -> inject0 $ f e
            Right e -> weaken e

-- | Remove the tag attached to the effect class.
untag ::
    forall tag e r f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (Tag e tag ': r) f ~> FreerEffects fr u (e ': r) f
untag = transform getTag

unembed ::
    forall g r f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (Embed g ': r) f ~> FreerEffects fr u (g ': r) f
unembed = transform unEmbed

-- | Interpose the effect class that exists within the effect class list.
interpose ::
    forall e es f fr u c.
    (TransFreer c fr, Union u, Member u e es, c f) =>
    (e ~> FreerEffects fr u es f) ->
    FreerEffects fr u es f ~> FreerEffects fr u es f
interpose f =
    overFreerEffects $ reinterpretFT \u ->
        case project @_ @e u of
            Just e -> unFreerEffects $ f e
            Nothing -> liftInsT u

-- | Interpose the effect class that exists within the effect class list using a monad transformer.
interposeT ::
    forall e t es m fr u.
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

{- |
Transform all other effect classes in the effect class list and the underlying carrier, along
with the effect class that exists within the effect class list, into any carrier @g@.
-}
interposeAll ::
    forall e g es f fr u c.
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

{- |
Interpose the effect class that exists within the effect class list using a delimited continuation.
-}
interposeK ::
    forall e r a es m fr u.
    (MonadTransFreer fr, Union u, Member u e es, Monad m) =>
    (a -> FreerEffects fr u es m r) ->
    (forall x. (x -> FreerEffects fr u es m r) -> e x -> FreerEffects fr u es m r) ->
    FreerEffects fr u es m a ->
    FreerEffects fr u es m r
interposeK k i = (`runContT` k) . interposeContT \e -> ContT (`i` e)
{-# INLINE interposeK #-}

{- |
Interpose the effect class that exists within the effect class list using a continuation monad
transformer.
-}
interposeContT ::
    forall e r es m fr u.
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

-- | Transform the effect of the effect class that exists within the effect class list.
intercept ::
    forall e es f fr u c.
    (TransFreer c fr, Union u, Member u e es, c f) =>
    (e ~> e) ->
    FreerEffects fr u es f ~> FreerEffects fr u es f
intercept f =
    overFreerEffects $ transformT \u ->
        case project @_ @e u of
            Just e -> inject $ f e
            Nothing -> u

-- | Insert an arbitrary effect class at the beginning of the effect class list.
raise ::
    forall e es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e ': es) f
raise = transformAll weaken
{-# INLINE raise #-}

-- | Insert two arbitrary effect classes at the beginning of the effect class list.
raise2 ::
    forall e1 e2 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': es) f
raise2 = transformAll weaken2
{-# INLINE raise2 #-}

-- | Insert three arbitrary effect classes at the beginning of the effect class list.
raise3 ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raise3 = transformAll weaken3
{-# INLINE raise3 #-}

-- | Insert four arbitrary effect classes at the beginning of the effect class list.
raise4 ::
    forall e1 e2 e3 e4 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u es f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise4 = transformAll weaken4
{-# INLINE raise4 #-}

-- | Insert an arbitrary effect class below the leading effect class in the effect class list.
raiseUnder ::
    forall e1 e2 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': es) f
raiseUnder = transformAll weakenUnder
{-# INLINE raiseUnder #-}

{- |
Insert an arbitrary effect class below the first two leading effect classes in the effect class
list.
-}
raiseUnder2 ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raiseUnder2 = transformAll weakenUnder2
{-# INLINE raiseUnder2 #-}

{- |
Insert an arbitrary effect class below the first three leading effect classes in the effect class list.
-}
raiseUnder3 ::
    forall e1 e2 e3 e4 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raiseUnder3 = transformAll weakenUnder3
{-# INLINE raiseUnder3 #-}

-- | Insert two arbitrary effect classes below the leading effect class in the effect class list.
raise2Under ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
raise2Under = transformAll weaken2Under
{-# INLINE raise2Under #-}

{- |
Insert two arbitrary effect classes below the first two leading effect classes in the effect class list.
-}
raise2Under2 ::
    forall e1 e2 e3 e4 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise2Under2 = transformAll weaken2Under2
{-# INLINE raise2Under2 #-}

-- | Inserts three arbitrary effect classes under the top effect class in the effect class list.
raise3Under ::
    forall e1 e2 e3 e4 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
raise3Under = transformAll weaken3Under
{-# INLINE raise3Under #-}

-- | Swaps the top two effect classes in the effect class list.
flipFreer ::
    forall e1 e2 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (e2 ': e1 ': es) f
flipFreer = transformAll flipUnion
{-# INLINE flipFreer #-}

-- | Reverses the order of the top three effect classes in the effect class list.
flipFreer3 ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e3 ': e2 ': e1 ': es) f
flipFreer3 = transformAll flipUnion3
{-# INLINE flipFreer3 #-}

-- | Swaps the second and third effect classes from the top in the effect class list.
flipFreerUnder ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e1 ': e3 ': e2 ': es) f
flipFreerUnder = transformAll flipUnionUnder
{-# INLINE flipFreerUnder #-}

-- | Rotates the top three effect classes in the effect class list to the left.
rotate3 ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e2 ': e3 ': e1 ': es) f
rotate3 = transformAll rot3
{-# INLINE rotate3 #-}

-- | Rotates the top three effect classes in the effect class list to the left twice.
rotate3' ::
    forall e1 e2 e3 es f fr u c.
    (TransFreer c fr, Union u, c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (e3 ': e1 ': e2 ': es) f
rotate3' = transformAll rot3'
{-# INLINE rotate3' #-}

-- | Bundles the top two effect classes in the effect class list into any open union.
bundle2 ::
    forall e1 e2 es f fr u c u'.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': es) f ~> FreerEffects fr u (u' '[e1, e2] ': es) f
bundle2 = transformAll bundleUnion2
{-# INLINE bundle2 #-}

-- | Bundles the top three effect classes in the effect class list into any open union.
bundle3 ::
    forall e1 e2 e3 u' es f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': es) f ~> FreerEffects fr u (u' '[e1, e2, e3] ': es) f
bundle3 = transformAll bundleUnion3
{-# INLINE bundle3 #-}

-- | Bundles the top four effect classes in the effect class list into any open union.
bundle4 ::
    forall e1 e2 e3 e4 u' es f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f ~> FreerEffects fr u (u' '[e1, e2, e3, e4] ': es) f
bundle4 = transformAll bundleUnion4
{-# INLINE bundle4 #-}

-- | Expands the open union at the top of the effect class list.
unbundle2 ::
    forall e1 e2 u' es f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2] ': es) f ~> FreerEffects fr u (e1 ': e2 ': es) f
unbundle2 = transformAll unbundleUnion2
{-# INLINE unbundle2 #-}

-- | Expands the open union at the top of the effect class list.
unbundle3 ::
    forall e1 e2 e3 u' es f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2, e3] ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': es) f
unbundle3 = transformAll unbundleUnion3
{-# INLINE unbundle3 #-}

-- | Expands the open union at the top of the effect class list.
unbundle4 ::
    forall e1 e2 e3 e4 u' es f fr u c.
    (TransFreer c fr, Union u, Union u', c f) =>
    FreerEffects fr u (u' '[e1, e2, e3, e4] ': es) f ~> FreerEffects fr u (e1 ': e2 ': e3 ': e4 ': es) f
unbundle4 = transformAll unbundleUnion4
{-# INLINE unbundle4 #-}

{- |
Transforms the lower carrier.

__Warning__: The given natural transformation must be a monad morphism
(see <https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html>).
If not, the result will be ill-behaved.
-}
hoistFreerEffects ::
    forall g f es fr u c.
    (TransFreer c fr, c f, c g) =>
    (f ~> g) ->
    FreerEffects fr u es f ~> FreerEffects fr u es g
hoistFreerEffects f = overFreerEffects $ hoistFreer f
{-# INLINE hoistFreerEffects #-}

-- | Converts the lower carrier to an instruction.
lowerToIns ::
    forall f g es fr u c.
    (TransFreer c fr, c g, c (f + g), Union u) =>
    FreerEffects fr u es (f + g) ~> FreerEffects fr u (f ': es) g
lowerToIns =
    overFreerEffects $
        interpretFT
            (caseF (liftInsT . inject0) liftLowerFT)
            (liftInsT . weaken)
{-# INLINE lowerToIns #-}

-- | Converts the instruction to the lower carrier.
insToLower ::
    forall f g es fr u c.
    (TransFreer c fr, c (f + g), c g, Union u) =>
    FreerEffects fr u (f ': es) g ~> FreerEffects fr u es (f + g)
insToLower = overFreerEffects $ interpretFT (liftLowerFT . R1) (liftLowerFT . L1 |+|: liftInsT)
{-# INLINE insToLower #-}

{- |
Interprets the lower carrier.

__Warning__: The given natural transformation must be a monad morphism
(see <https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html>).
If not, the result will be ill-behaved.
-}
interpretLower ::
    forall g f es fr u c.
    (TransFreer c fr, c f, c g) =>
    (f ~> FreerEffects fr u es g) ->
    FreerEffects fr u es f ~> FreerEffects fr u es g
interpretLower f = overFreerEffects $ interposeLowerT (unFreerEffects . f)
{-# INLINE interpretLower #-}

-- | Accesses the inside of the 'FreerEffects' wrapper.
overFreerEffects ::
    forall b es' g fr' u' a es f fr u.
    (fr (u es) f a -> fr' (u' es') g b) ->
    FreerEffects fr u es f a ->
    FreerEffects fr' u' es' g b
overFreerEffects f = freerEffects . f . unFreerEffects
{-# INLINE overFreerEffects #-}

-- | Drops a Freer with no effect classes to interpret to the lower carrier.
interpreted ::
    forall f fr u c.
    (TransFreer c fr, c f, Union u) =>
    FreerEffects fr u '[] f ~> f
interpreted = runInterpretF absurdUnion . unFreerEffects
{-# INLINE interpreted #-}

-- | Splits the Freer into the lower carrier.
splitFreerEffects ::
    forall e fr' es f fr u c.
    (TransFreer c fr', TransFreer c fr, c f, c (FreerEffects fr u es f), Union u) =>
    FreerEffects fr u (e ': es) f ~> fr' e (FreerEffects fr u es f)
splitFreerEffects a =
    unFreerEffects a & interpretFT (liftLowerFT . freerEffects . liftLowerFT) \u ->
        case decomp u of
            Left e -> liftInsT e
            Right e -> liftLowerFT $ freerEffects $ liftInsT e

-- | Transfer the effect to the underlying level.
subsume ::
    forall e es f fr u c.
    (TransFreer c fr, SendIns e (FreerEffects fr u es f), Union u, c f) =>
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u es f
subsume = interpret sendIns
{-# INLINE subsume #-}

-- | Transfer the effect to the lower carrier.
subsumeLower ::
    forall e f es fr u c.
    (TransFreer c fr, SendIns e f, Union u, c f) =>
    FreerEffects fr u (e ': es) f ~> FreerEffects fr u es f
subsumeLower = interpret $ liftLower . sendIns
{-# INLINE subsumeLower #-}

-- | Lifts the lower carrier.
liftLower ::
    forall f es fr u c.
    (TransFreer c fr, c f) =>
    f ~> FreerEffects fr u es f
liftLower = freerEffects . liftLowerFT
{-# INLINE liftLower #-}

-- | Embeds an IO action into a lower carrier that is a `MonadIO`.
runIO :: MonadIO m => Fre (IO ': es) m ~> Fre es m
runIO = interpret $ liftLower . liftIO
{-# INLINE runIO #-}

-- | Interprets all effects in the effect class list at once.
runInterpret ::
    forall f es fr u c.
    (TransFreer c fr, c f) =>
    (u es ~> f) ->
    FreerEffects fr u es f ~> f
runInterpret f = runInterpretF f . unFreerEffects
{-# INLINE runInterpret #-}

-- | Drops the Freer to the lower carrier.
runFreerEffects ::
    forall f fr u c.
    (TransFreer c fr, c f, Union u) =>
    FreerEffects fr u '[f] f ~> f
runFreerEffects = runInterpret $ id |+|: absurdUnion
{-# INLINE runFreerEffects #-}

-- | A type synonym for commonly used Monad Freer.
type Fre es f = FreerEffects FreerChurchT ExtensibleUnion es f

-- -- | Type synonym for commonly used Applicative Freer.
-- type FreA es f = FreerEffects (FreerFinalT Applicative) SumUnion es f

-- | An operator representing the membership relationship of the effect class list.
type e <| es = Member ExtensibleUnion e es

type eci <|- es = MemberDep ExtensibleUnion eci es
