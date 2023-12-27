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

A Heftia carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
module Control.Effect.Hefty where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns), NopI, NopS, SendIns, SendSig, sendIns, sendSig)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:))
import Control.Freer (Freer, InsClass, interpretFreer, liftIns, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, overHefty, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Free.Sum (caseF, pattern L1, pattern R1, type (+))
import Data.Hefty.Union (
    ForallHFunctor,
    HFunctorUnion,
    MemberRec,
    Union,
    absurdUnion,
    decomp,
    injectRec,
    weaken,
    weakenUnder,
 )
import Data.Kind (Type)

{- |
A common wrapper data type for representing first-order & higher-order effectful programs.
-}
newtype
    Effectful
        (u :: [SigClass] -> SigClass)
        (f :: InsClass -> Type -> Type)
        (eh :: SigClass)
        (ef :: InsClass)
        (a :: Type) = Effectful {unEffectful :: Hefty f (EffUnion u eh ef) a}

-- | Open union for higher-order effect classes and first-order effect classes.
newtype EffUnion u eh ef f a = EffUnion
    {unEffUnion :: (EffUnionH u (NormalizeSig eh) f + SumToUnionF u ef) a}

-- | A wrapper to provide an instance of t'HFunctor' for the open union.
newtype EffUnionH (u :: [SigClass] -> SigClass) (e :: SigClass) f a = EffUnionH
    {unEffUnionH :: SumToUnion u e f a}

-- | Manipulate the inside of the t'Effectful' wrapper.
overEffectful ::
    forall b eh' ef' f' u' a eh ef f u.
    (Hefty f (EffUnion u eh ef) a -> Hefty f' (EffUnion u' eh' ef') b) ->
    Effectful u f eh ef a ->
    Effectful u' f' eh' ef' b
overEffectful f = Effectful . f . unEffectful
{-# INLINE overEffectful #-}

infixr 1 ~>

-- | A natural transformation.
type f ~> g = forall x. f x -> g x

-- | t'HFunctor' constraint for effect class open unions.
type HFunctors u e = HFunctor (SumToUnionH u e)

{- |
A version of t'HFunctors' where t'HFunctor' is derived for unions extended in the head direction by
t'HFunctorUnion'.
-}
type TailHFunctor u e = ForallHFunctor u (SumToUnionList u (NormalizeSig e))

-- | t'HFunctor' constraint for effect classes that are either single or in an open union.
type HeadHFunctor u e = HFunctor (MultiToUnionH u e)

instance HFunctors u eh => HFunctor (EffUnion u eh ef) where
    hfmap f = EffUnion . caseF (L1 . hfmap f) R1 . unEffUnion
    {-# INLINE hfmap #-}

deriving newtype instance HFunctor (SumToUnion u e) => HFunctor (EffUnionH u e)

{- |
Convert a given list of higher-order effect classes into a suitable representation type for each
case of being empty, single, or multiple.
-}
type family MultiListToUnion (u :: [SigClass] -> SigClass) (es :: [SigClass]) :: SigClass where
    MultiListToUnion u '[] = NopS
    MultiListToUnion u '[e] = e
    MultiListToUnion u es = u es

{- |
Recursively decompose the sum of higher-order effects into a list, following the direction of right
association.
-}
type family SumToUnionList (u :: [SigClass] -> SigClass) (e :: SigClass) :: [SigClass] where
    SumToUnionList u (e1 :+: e2) = MultiToUnion u e1 ': SumToUnionList u e2
    SumToUnionList u NopS = '[]
    SumToUnionList u (SingleSig e) = '[e]

-- | Convert the sum of higher-order effect classes into an open union without normalization.
type SumToUnion u e = u (SumToUnionList u e)

-- | Convert the sum of higher-order effect classes into an open union with normalization.
type SumToUnionH u e = SumToUnion u (NormalizeSig e)

-- | Convert the sum of first-order effect classes into an open union with normalization.
type SumToUnionF u e = SumToUnionH u (LiftIns e) NopI

{- |
Convert the sum of higher-order effects into an open union without normalization.

If it's a single higher-order effect class rather than a sum, leave it as is without converting.
-}
type MultiToUnion u e = MultiListToUnion u (SumToUnionList u e)

{- |
Convert the sum of higher-order effect classes into an open union with normalization.

If it's a single higher-order effect rather than a sum, leave it as is without converting.
-}
type MultiToUnionH u e = MultiToUnion u (NormalizeSig e)

{- |
Convert the sum of first-order effect classes into an open union with normalization.

If it's a single first-order effect class rather than a sum, leave it as is without converting.
-}
type MultiToUnionF u e = MultiToUnionH u (LiftIns e) NopI

{- |
Normalization in preparation for decomposing the sum of effect classes into a list.

In particular, mark an indivisible, single effect class by applying the t'SingleSig' wrapper to it.
-}
type family NormalizeSig e where
    NormalizeSig (LiftIns NopI) = LiftIns NopI
    NormalizeSig (LiftIns (e1 + e2)) = NormalizeSig (LiftIns e1) :+: NormalizeSig (LiftIns e2)
    NormalizeSig (e1 :+: e2) = NormalizeSig e1 :+: NormalizeSig e2
    NormalizeSig e = SingleSig e

{- |
A wrapper to mark a single, i.e., a higher-order effect class that cannot be further decomposed as
a sum.
-}
newtype SingleSig (e :: SigClass) f a = SingleSig {unSingleSig :: e f a}
    deriving newtype (HFunctor)

deriving newtype instance Functor (Hefty f (EffUnion u eh ef)) => Functor (Effectful u f eh ef)
deriving newtype instance Applicative (Hefty f (EffUnion u eh ef)) => Applicative (Effectful u f eh ef)
deriving newtype instance Alternative (Hefty f (EffUnion u eh ef)) => Alternative (Effectful u f eh ef)
deriving newtype instance Monad (Hefty f (EffUnion u eh ef)) => Monad (Effectful u f eh ef)
deriving newtype instance MonadPlus (Hefty f (EffUnion u eh ef)) => MonadPlus (Effectful u f eh ef)
deriving newtype instance (MonadBase b (Hefty f (EffUnion u eh ef)), Monad b) => MonadBase b (Effectful u f eh ef)
deriving newtype instance MonadIO (Hefty f (EffUnion u eh ef)) => MonadIO (Effectful u f eh ef)
deriving newtype instance MonadFail (Hefty f (EffUnion u eh ef)) => MonadFail (Effectful u f eh ef)

deriving stock instance Foldable (Hefty f (EffUnion u eh ef)) => Foldable (Effectful u f eh ef)
deriving stock instance Traversable (Hefty f (EffUnion u eh ef)) => Traversable (Effectful u f eh ef)

deriving newtype instance Eq (Hefty f (EffUnion u eh ef) a) => Eq (Effectful u f eh ef a)
deriving newtype instance Ord (Hefty f (EffUnion u eh ef) a) => Ord (Effectful u f eh ef a)
deriving newtype instance Read (Hefty f (EffUnion u eh ef) a) => Read (Effectful u f eh ef a)
deriving newtype instance Show (Hefty f (EffUnion u eh ef) a) => Show (Effectful u f eh ef a)

type MemberF u e ef = MemberH u (LiftIns e) (LiftIns ef)

instance (MemberF u e ef, Freer c fr) => SendIns e (Effectful u fr eh ef) where
    sendIns = Effectful . Hefty . liftIns . EffUnion . R1 . injectRec . LiftIns
    {-# INLINE sendIns #-}

type MemberH u e eh = MemberRec u e (SumToUnionList u (NormalizeSig eh))

-- enhance: introduce 'HFunctorCoercible' for performance
instance (MemberH u e eh, Freer c fr, HFunctor e) => SendSig e (Effectful u fr eh ef) where
    sendSig =
        Effectful . Hefty . liftIns . EffUnion . L1 . EffUnionH . injectRec . hfmap unEffectful
    {-# INLINE sendSig #-}

-- | Using the provided interpretation function, interpret first-order effects.
interpret ::
    forall e r f u c.
    (Freer c f, Union u) =>
    (MultiToUnionF u e ~> Effectful u f NopS r) ->
    Effectful u f NopS (e + r) ~> Effectful u f NopS r
interpret i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseF
            (absurdUnion . unEffUnionH)
            ( \u -> case decomp u of
                Left e -> unHefty $ unEffectful $ i e
                Right e -> liftIns $ EffUnion $ R1 e
            )
            . unEffUnion

{- |
Using the provided interpretation function, interpret first-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRec ::
    forall e eh r f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    (MultiToUnionF u e ~> Effectful u f eh r) ->
    Effectful u f eh (e + r) ~> Effectful u f eh r
interpretRec i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseF
            ( liftIns
                . EffUnion
                . L1
                . hfmap (unEffectful . interpretRec i . Effectful)
            )
            ( \u -> case decomp u of
                Left e -> unHefty $ unEffectful $ i e
                Right e -> liftIns $ EffUnion $ R1 e
            )
            . unEffUnion

{- |
Using the provided interpretation function, interpret higher-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRecH ::
    forall e r ef f u c.
    (Freer c f, Union u, HeadHFunctor u e, HFunctors u r) =>
    (MultiToUnionH u e (Effectful u f r ef) ~> Effectful u f r ef) ->
    Effectful u f (e :+: r) ef ~> Effectful u f r ef
interpretRecH i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseF
            ( \(EffUnionH u) -> case decomp u of
                Left e ->
                    unHefty $ unEffectful $ i $ hfmap int e
                Right e ->
                    liftIns $ EffUnion $ L1 $ EffUnionH $ hfmap (unEffectful . int) e
            )
            (liftIns . EffUnion . R1)
            . unEffUnion
  where
    int :: Hefty f (EffUnion u (e :+: r) ef) ~> Effectful u f r ef
    int = interpretRecH i . Effectful
    {-# INLINE int #-}

reinterpret ::
    forall e r f u c.
    (Freer c f, HFunctorUnion u) =>
    (MultiToUnionF u e ~> Effectful u f NopS (e + r)) ->
    Effectful u f NopS (e + r) ~> Effectful u f NopS (e + r)
reinterpret f = interpret f . raiseUnder
{-# INLINE reinterpret #-}

reinterpretRec ::
    forall e eh r f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    (MultiToUnionF u e ~> Effectful u f eh (e + r)) ->
    Effectful u f eh (e + r) ~> Effectful u f eh (e + r)
reinterpretRec f = interpretRec f . raiseUnder
{-# INLINE reinterpretRec #-}

reinterpretRecH ::
    forall e r ef f u c.
    (Freer c f, HFunctorUnion u, HeadHFunctor u e, TailHFunctor u r) =>
    (MultiToUnionH u e (Effectful u f (e :+: r) ef) ~> Effectful u f (e :+: r) ef) ->
    Effectful u f (e :+: r) ef ~> Effectful u f (e :+: r) ef
reinterpretRecH f = interpretRecH f . raiseUnderH
{-# INLINE reinterpretRecH #-}

transformAllFH ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    ( SumToUnionH u eh (Hefty f (EffUnion u eh' ef'))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef'))
    ) ->
    (SumToUnionF u ef ~> SumToUnionF u ef') ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
transformAllFH fh ff =
    overEffectful
        . overHefty
        $ transformFreer
        $ EffUnion
            . caseF
                ( L1
                    . EffUnionH
                    . fh
                    . hfmap (unEffectful . transformAllFH fh ff . Effectful)
                    . unEffUnionH
                )
                (R1 . ff)
            . unEffUnion

transformAll ::
    forall ef' ef eh f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    (SumToUnionF u ef ~> SumToUnionF u ef') ->
    Effectful u f eh ef ~> Effectful u f eh ef'
transformAll = transformAllFH id
{-# INLINE transformAll #-}

transformAllH ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    ( SumToUnionH u eh (Hefty f (EffUnion u eh' ef))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef))
    ) ->
    Effectful u f eh ef ~> Effectful u f eh' ef
transformAllH f = transformAllFH f id
{-# INLINE transformAllH #-}

raise ::
    forall e1 e eh f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    Effectful u f eh e ~> Effectful u f eh (e1 + e)
raise = transformAll weaken
{-# INLINE raise #-}

raiseH ::
    forall e1 e ef f u c.
    (Freer c f, Union u, HFunctors u e) =>
    Effectful u f e ef ~> Effectful u f (e1 :+: e) ef
raiseH = transformAllH weaken
{-# INLINE raiseH #-}

raiseUnder ::
    forall e1 e2 e eh f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    Effectful u f eh (e1 + e) ~> Effectful u f eh (e1 + e2 + e)
raiseUnder = transformAll weakenUnder
{-# INLINE raiseUnder #-}

raiseUnderH ::
    forall e1 e2 e ef f u c.
    ( Freer c f
    , Union u
    , HeadHFunctor u e1
    , HFunctors u (e1 :+: e)
    , HFunctors u (e1 :+: e2 :+: e)
    ) =>
    Effectful u f (e1 :+: e) ef ~> Effectful u f (e1 :+: e2 :+: e) ef
raiseUnderH = transformAllH weakenUnder
{-# INLINE raiseUnderH #-}
