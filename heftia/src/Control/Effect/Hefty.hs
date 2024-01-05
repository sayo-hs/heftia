{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Heftia carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
module Control.Effect.Hefty where

import Control.Applicative (Alternative)
import Control.Arrow ((>>>))
import Control.Effect.Class (
    LiftIns (LiftIns),
    NopI,
    NopS,
    SendIns,
    SendSig,
    sendIns,
    sendSig,
    unliftIns,
 )
import Control.Effect.Class.Machinery.HFunctor (HFunctor, caseH, hfmap, (:+:) (Inl, Inr))
import Control.Freer (Freer, InsClass, interpretFreer, liftIns, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, overHefty, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Free.Sum (type (+), type (~>))
import Data.Hefty.Union (
    ForallHFunctor,
    HFunctorUnion,
    MemberRec,
    Union,
    absurdUnion,
    decomp,
    inject0,
    injectRec,
    weaken,
    weakenUnder,
 )
import Data.Kind (Type)

{- |
A common wrapper data type for representing first-order & higher-order extensible effectful
programs.
-}
newtype
    Effectful
        (u :: [SigClass] -> SigClass)
        (f :: InsClass -> Type -> Type)
        (eh :: SigClass)
        (ef :: InsClass)
        (a :: Type) = Effectful {unEffectful :: Hefty f (EffUnion u eh ef) a}

-- | Open union for first-order & higher-order effect classes.
type EffUnion u eh ef = SumToUnion u (NormalizeSig eh) :+: LiftIns (SumToUnionF u ef)

-- | Manipulate the inside of the t'Effectful' wrapper.
overEffectful ::
    forall b eh' ef' f' u' a eh ef f u.
    (Hefty f (EffUnion u eh ef) a -> Hefty f' (EffUnion u' eh' ef') b) ->
    Effectful u f eh ef a ->
    Effectful u' f' eh' ef' b
overEffectful f = Effectful . f . unEffectful
{-# INLINE overEffectful #-}

infixr 4 $
infixr 5 $$

-- | Type-level infix applcation for functors.
type (f :: Type -> Type) $ a = f a

-- | Type-level infix applcation for higher-order functors.
type (h :: (Type -> Type) -> Type -> Type) $$ f = h f

-- | t'HFunctor' constraint for effect class open unions.
type HFunctors u e = HFunctor (SumToUnionH u e)

{- |
A version of t'HFunctors' where t'HFunctor' is derived for unions extended in the head direction by
t'HFunctorUnion'.
-}
type TailHFunctor u e = ForallHFunctor u (SumToUnionList u (NormalizeSig e))

-- | t'HFunctor' constraint for effect classes that are either single or in an open union.
type HeadHFunctor u e = HFunctor (MultiToUnionH u e)

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

{- |
The higher-order effect class @er@ can be decomposed into the sum of t'EffHead' @er@ and t'EffTail'
@er@ (@er ≅ EffHead er :+: EffTail er@).
-}
type DecompH u er =
    SumToUnionList u (NormalizeSig er)
        ~ MultiToUnionH u (EffHead er)
        ': SumToUnionList u (NormalizeSig (EffTail er))

{- |
The first-order effect class @er@ can be decomposed into the sum of t'EffHeadF' @er@ and t'EffTailF'
@er@ (@er ≅ EffHeadF er + EffTailF er@).
-}
type DecompF u er = DecompH u (LiftIns er)

type family EffHead e where
    EffHead (LiftIns e) = LiftIns (EffHeadF e)
    EffHead (e :+: r) = e
    EffHead e = e

type family EffHeadF e where
    EffHeadF (e + r) = e
    EffHeadF e = e

type family EffTail e where
    EffTail (LiftIns e) = LiftIns (EffTailF e)
    EffTail (e :+: r) = r
    EffTail e = NopS

type family EffTailF e where
    EffTailF (e + r) = r
    EffTailF e = NopI

compEff ::
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    Effectful u f eh (EffHeadF er + EffTailF er) ~> Effectful u f eh er
compEff = coerce
{-# INLINE compEff #-}

compEffH ::
    (Freer c f, Union u, HFunctors u er, DecompH u er) =>
    Effectful u f (EffHead er :+: EffTail er) ef ~> Effectful u f er ef
compEffH = coerce
{-# INLINE compEffH #-}

decompEff ::
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    Effectful u f eh er ~> Effectful u f eh (EffHeadF er + EffTailF er)
decompEff = coerce
{-# INLINE decompEff #-}

decompEffH ::
    (Freer c f, Union u, HFunctors u er, DecompH u er) =>
    Effectful u f er ef ~> Effectful u f (EffHead er :+: EffTail er) ef
decompEffH = coerce
{-# INLINE decompEffH #-}

deriving newtype instance Functor (Hefty f (EffUnion u eh ef)) => Functor (Effectful u f eh ef)
deriving newtype instance
    Applicative (Hefty f (EffUnion u eh ef)) =>
    Applicative (Effectful u f eh ef)
deriving newtype instance
    Alternative (Hefty f (EffUnion u eh ef)) =>
    Alternative (Effectful u f eh ef)
deriving newtype instance Monad (Hefty f (EffUnion u eh ef)) => Monad (Effectful u f eh ef)
deriving newtype instance MonadPlus (Hefty f (EffUnion u eh ef)) => MonadPlus (Effectful u f eh ef)
deriving newtype instance
    (MonadBase b (Hefty f (EffUnion u eh ef)), Monad b) =>
    MonadBase b (Effectful u f eh ef)
deriving newtype instance MonadIO (Hefty f (EffUnion u eh ef)) => MonadIO (Effectful u f eh ef)
deriving newtype instance MonadFail (Hefty f (EffUnion u eh ef)) => MonadFail (Effectful u f eh ef)

deriving stock instance Foldable (Hefty f (EffUnion u eh ef)) => Foldable (Effectful u f eh ef)
deriving stock instance
    Traversable (Hefty f (EffUnion u eh ef)) =>
    Traversable (Effectful u f eh ef)

deriving newtype instance Eq (Hefty f (EffUnion u eh ef) a) => Eq (Effectful u f eh ef a)
deriving newtype instance Ord (Hefty f (EffUnion u eh ef) a) => Ord (Effectful u f eh ef a)
deriving newtype instance Read (Hefty f (EffUnion u eh ef) a) => Read (Effectful u f eh ef a)
deriving newtype instance Show (Hefty f (EffUnion u eh ef) a) => Show (Effectful u f eh ef a)

type MemberF u e ef = MemberH u (LiftIns e) (LiftIns ef)

instance (MemberF u e ef, Freer c fr) => SendIns e (Effectful u fr eh ef) where
    sendIns = Effectful . Hefty . liftIns . Inr . LiftIns . injectRec . LiftIns
    {-# INLINE sendIns #-}

type MemberH u e eh = MemberRec u e (SumToUnionList u (NormalizeSig eh))

-- enhance: introduce 'HFunctorCoercible' for performance
instance (MemberH u e eh, Freer c fr, HFunctor e) => SendSig e (Effectful u fr eh ef) where
    sendSig =
        Effectful . Hefty . liftIns . Inl . injectRec . hfmap unEffectful
    {-# INLINE sendSig #-}

-- | Using the provided interpretation function, interpret first-order effects.
interpret ::
    forall er f u c.
    (Freer c f, Union u, DecompF u er) =>
    MultiToUnionF u (EffHeadF er) ~> Effectful u f NopS (EffTailF er) ->
    Effectful u f NopS er ~> Effectful u f NopS (EffTailF er)
interpret i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseH
            absurdUnion
            ( unliftIns
                >>> decomp
                >>> caseH
                    (unHefty . unEffectful . i)
                    (liftIns . Inr . LiftIns)
            )

{- |
Using the provided interpretation function, interpret first-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRec ::
    forall er eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    MultiToUnionF u (EffHeadF er) ~> Effectful u f eh (EffTailF er) ->
    Effectful u f eh er ~> Effectful u f eh (EffTailF er)
interpretRec i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseH
            ( liftIns
                . Inl
                . hfmap (unEffectful . interpretRec @er i . Effectful)
            )
            ( unliftIns
                >>> decomp
                >>> caseH
                    (unHefty . unEffectful . i)
                    (liftIns . Inr . LiftIns)
            )

{- |
Using the provided interpretation function, interpret higher-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRecH ::
    forall er ef f u c.
    (Freer c f, Union u, HeadHFunctor u (EffHead er), HFunctors u (EffTail er), DecompH u er) =>
    MultiToUnionH u (EffHead er) (Effectful u f (EffTail er) ef) ~> Effectful u f (EffTail er) ef ->
    Effectful u f er ef ~> Effectful u f (EffTail er) ef
interpretRecH i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseH
            ( decomp
                >>> caseH
                    (unHefty . unEffectful . i . hfmap int)
                    (liftIns . Inl . hfmap (unEffectful . int))
            )
            (liftIns . Inr . coerce)
  where
    int :: Hefty f (EffUnion u er ef) ~> Effectful u f (EffTail er) ef
    int = interpretRecH @er i . Effectful
    {-# INLINE int #-}

reinterpret ::
    forall er f u c.
    (Freer c f, HFunctorUnion u, DecompF u er) =>
    MultiToUnionF u (EffHeadF er) ~> Effectful u f NopS er ->
    Effectful u f NopS er ~> Effectful u f NopS er
reinterpret f = interpret f . raiseUnder . decompEff
{-# INLINE reinterpret #-}

reinterpretRec ::
    forall er eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    MultiToUnionF u (EffHeadF er) ~> Effectful u f eh er ->
    Effectful u f eh er ~> Effectful u f eh er
reinterpretRec f = interpretRec f . raiseUnder . decompEff
{-# INLINE reinterpretRec #-}

reinterpretRecH ::
    forall er ef f u c.
    ( Freer c f
    , HFunctorUnion u
    , DecompH u er
    , HeadHFunctor u (EffHead er)
    , TailHFunctor u (EffTail er)
    ) =>
    MultiToUnionH u (EffHead er) (Effectful u f er ef) ~> Effectful u f er ef ->
    Effectful u f er ef ~> Effectful u f er ef
reinterpretRecH f = interpretRecH f . raiseUnderH . decompEffH
{-# INLINE reinterpretRecH #-}

transformAllFH ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh' ef'))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef')) ->
    (SumToUnionF u ef ~> SumToUnionF u ef') ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
transformAllFH fh ff =
    overEffectful
        . overHefty
        $ transformFreer
        $ caseH
            (Inl . fh . hfmap (unEffectful . transformAllFH @eh' @ef' @eh @ef fh ff . Effectful))
            (Inr . LiftIns . ff . unliftIns)

transformAll ::
    forall ef' ef eh f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionF u ef ~> SumToUnionF u ef' ->
    Effectful u f eh ef ~> Effectful u f eh ef'
transformAll = transformAllFH id
{-# INLINE transformAll #-}

transformAllH ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh' ef))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef)) ->
    Effectful u f eh ef ~> Effectful u f eh' ef
transformAllH f = transformAllFH f id
{-# INLINE transformAllH #-}

raise ::
    forall er eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    Effectful u f eh (EffTailF er) ~> Effectful u f eh er
raise = transformAll weaken
{-# INLINE raise #-}

raiseH ::
    forall er ef f u c.
    (Freer c f, Union u, HFunctors u (EffTail er), DecompH u er) =>
    Effectful u f (EffTail er) ef ~> Effectful u f er ef
raiseH = transformAllH weaken
{-# INLINE raiseH #-}

raiseUnder ::
    forall e1r e2 eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u e1r) =>
    Effectful u f eh (e2 + EffTailF e1r) ~> Effectful u f eh (e2 + e1r)
raiseUnder = transformAll weakenUnder
{-# INLINE raiseUnder #-}

raiseUnderH ::
    forall e1r e2 ef f u c.
    ( Freer c f
    , HFunctorUnion u
    , DecompH u e1r
    , HeadHFunctor u e2
    , TailHFunctor u (EffTail e1r)
    ) =>
    Effectful u f (e2 :+: EffTail e1r) ef ~> Effectful u f (e2 :+: e1r) ef
raiseUnderH = transformAllH weakenUnder
{-# INLINE raiseUnderH #-}

send0 ::
    ( Freer c f
    , Union u
    , DecompF u er
    , NormalizeSig (LiftIns (EffHeadF er)) ~ SingleSig (LiftIns (EffHeadF er))
    ) =>
    EffHeadF er ~> Effectful u f eh er
send0 = Effectful . Hefty . liftIns . Inr . LiftIns . inject0 . LiftIns
