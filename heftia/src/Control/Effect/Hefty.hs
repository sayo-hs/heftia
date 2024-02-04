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
import Control.Effect (SendIns, SendSig, sendIns, sendSig, type (~>))
import Control.Freer (Freer, InsClass, interpretFreer, liftIns, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, overHefty, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (Cont, ContT (ContT), lift, runContT)
import Control.Monad.Freer (MonadFreer, interpretFreerK)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Trans (MonadTrans)
import Data.Coerce (coerce)
import Data.Effect (LNop, LiftIns (LiftIns), Nop, unliftIns)
import Data.Effect.HFunctor (HFunctor, caseH, hfmap, (:+:) (Inl, Inr))
import Data.Free.Sum (pattern L1, pattern R1, type (+))
import Data.Function ((&))
import Data.Hefty.Union (
    ForallHFunctor,
    HFunctorUnion,
    HasMembership,
    MemberRec,
    Union,
    exhaust,
    inject0,
    injectRec,
    projectRec,
    weaken,
    weakenUnder,
    (|+:),
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
type TailHFunctor u e = ForallHFunctor u (SumToUnionListN u e)

-- | t'HFunctor' constraint for effect classes that are either single or in an open union.
type HeadHFunctor u e = HFunctor (MultiToUnionN u e)

{- |
Convert a given list of higher-order effect classes into a suitable representation type for each
case of being empty, single, or multiple.
-}
type family MultiListToUnion (u :: [SigClass] -> SigClass) (es :: [SigClass]) :: SigClass where
    MultiListToUnion u '[] = LNop
    MultiListToUnion u '[e] = e
    MultiListToUnion u es = u es

{- |
Recursively decompose the sum of higher-order effects into a list, following the direction of right
association.
-}
type family SumToUnionList (u :: [SigClass] -> SigClass) (e :: SigClass) :: [SigClass] where
    SumToUnionList u (e1 :+: e2) = MultiToUnion u e1 ': SumToUnionList u e2
    SumToUnionList u LNop = '[]
    SumToUnionList u (SingleSig e) = '[e]

-- | Convert the sum of higher-order effect classes into an open union without normalization.
type SumToUnion u e = u (SumToUnionList u e)

-- | Convert the sum of higher-order effect classes into an open union with normalization.
type SumToUnionH u e = SumToUnion u (NormalizeSig e)

-- | Convert the sum of first-order effect classes into an open union with normalization.
type SumToUnionF u e = SumToUnionH u (LiftIns e) Nop

type SumToUnionListN u e = SumToUnionList u (NormalizeSig e)

{- |
Convert the sum of higher-order effects into an open union without normalization.

If it's a single higher-order effect class rather than a sum, leave it as is without converting.
-}
type MultiToUnion u e = MultiListToUnion u (SumToUnionList u e)

{- |
Convert the sum of higher-order effect classes into an open union with normalization.

If it's a single higher-order effect rather than a sum, leave it as is without converting.
-}
type MultiToUnionN u e = MultiToUnion u (NormalizeSig e)

{- |
Normalization in preparation for decomposing the sum of effect classes into a list.

In particular, mark an indivisible, single effect class by applying the t'SingleSig' wrapper to it.
-}
type family NormalizeSig e where
    NormalizeSig (LiftIns Nop) = LiftIns Nop
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
    SumToUnionListN u er
        ~ HeadMultiToUnionH u er
        ': SumToUnionListN u (EffTail er)

type HeadMultiToUnionH u er = MultiToUnionN u (EffHead er)

{- |
The first-order effect class @er@ can be decomposed into the sum of t'EffHeadF' @er@ and t'EffTailF'
@er@ (@er ≅ EffHeadF er + EffTailF er@).
-}
type DecompF u er =
    ( SumToUnionListN u (LiftIns er)
        ~ LHeadMultiToUnionF u er
        ': SumToUnionListN u (LiftIns (EffTailF er))
    , LiftInsIfSingle (HeadMultiToUnionF u er) (LHeadMultiToUnionF u er)
    )

type HeadMultiToUnionF u er = UnliftSingleLiftIns (LHeadMultiToUnionF u er)
type LHeadMultiToUnionF u er = MultiToUnionN u (LiftIns (EffHeadF er))

type family UnliftSingleLiftIns e where
    UnliftSingleLiftIns (LiftIns e) = e
    UnliftSingleLiftIns e = e Nop

class LiftInsIfSingle e le where
    liftInsIfSingle :: e ~> le Nop
    unliftInsIfSingle :: le Nop ~> e

instance LiftInsIfSingle (e Nop) e where
    liftInsIfSingle = id
    unliftInsIfSingle = id
    {-# INLINE liftInsIfSingle #-}
    {-# INLINE unliftInsIfSingle #-}

instance LiftInsIfSingle e (LiftIns e) where
    liftInsIfSingle = LiftIns
    unliftInsIfSingle = unliftIns
    {-# INLINE liftInsIfSingle #-}
    {-# INLINE unliftInsIfSingle #-}

type family EffHead e where
    EffHead (e :+: r) = e
    EffHead e = e

type family EffHeadF e where
    EffHeadF (e + r) = e
    EffHeadF e = e

type family EffTail e where
    EffTail (e :+: r) = r
    EffTail e = LNop

type family EffTailF e where
    EffTailF (e + r) = r
    EffTailF e = Nop

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
type HasMembershipF u e ef =
    HasMembership u (LiftIns e) (SumToUnionListN u (LiftIns ef))

instance (MemberF u e ef, Freer c fr) => SendIns e (Effectful u fr eh ef) where
    sendIns = liftFreer . liftIns . Inr . LiftIns . injectRec . LiftIns
    {-# INLINE sendIns #-}

type MemberH u e eh = MemberRec u e (SumToUnionListN u eh)

-- enhance: introduce 'HFunctorCoercible' for performance
instance (MemberH u e eh, Freer c fr, HFunctor e) => SendSig e (Effectful u fr eh ef) where
    sendSig = liftFreer . liftIns . Inl . injectRec . hfmap unEffectful
    {-# INLINE sendSig #-}

{-  all types of interpret-family functions:
        - interpret   :                 e  ~> E r           ->    E (e + r)  ~> E r
        - intercept   :                 e  ~> E (e + r)     ->    E (e + r)  ~> E (e + r)
        - reinterpret :                 e1 ~> E (e2 + r)    ->    E (e1 + r) ~> E (e2 + r)
        - interpose   :  e <| es  =>    e  ~> E es          ->    E es       ~> E es

    all possible suffix patterns of interpret-family functions:
        - <none>
        - H
        - H_
        - Rec
        - RecH
        - K
        - KH
        - KH_
        - ContT
        - ContTH
        - ContTH_
        - T
        - TH
        - TH_

    all types of transform-family functions:
        - transform :                  e1 ~> e2    ->    E (e1 + r) ~> E (e2 + r)
        - translate :  e2 <| r   =>    e1 ~> e2    ->    E (e1 + r) ~> E r
        - rewrite   :  e  <| es  =>    e  ~> e     ->    E es       ~> E es

    all possible suffix patterns of transform-family functions:
        - <none>
        - H
-}

-- todo: change 'Effectful' into type synonym and 'EffUnion' into newtype wrapper to reduce requiring of HFunctor constraints

-- | Using the provided interpretation function, interpret first-order effects.
interpret ::
    forall er f u c.
    (Freer c f, Union u, DecompF u er) =>
    HeadMultiToUnionF u er ~> Effectful u f LNop (EffTailF er) ->
    Effectful u f LNop er ~> Effectful u f LNop (EffTailF er)
interpret i =
    interpretAllE $ i . unliftInsIfSingle |+: injectF

interpretH ::
    forall eh ef f u c.
    (Freer c f, HFunctorUnion u, DecompH u eh, EffTail eh ~ LNop, HeadHFunctor u (EffHead eh)) =>
    (HeadMultiToUnionH u eh (Effectful u f LNop ef) ~> Effectful u f LNop ef) ->
    Effectful u f eh ef ~> Effectful u f LNop ef
interpretH i = interpretAllH $ i |+: exhaust
{-# INLINE interpretH #-}

interpretH_ ::
    forall eh ef f u c.
    (Freer c f, Union u, DecompH u eh, EffTail eh ~ LNop) =>
    (HeadMultiToUnionH u eh (Hefty f (EffUnion u eh ef)) ~> Effectful u f LNop ef) ->
    Effectful u f eh ef ~> Effectful u f LNop ef
interpretH_ i = interpretAllH_ $ i |+: exhaust
{-# INLINE interpretH_ #-}

{- |
Using the provided interpretation function, interpret first-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRec ::
    forall er eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    HeadMultiToUnionF u er ~> Effectful u f eh (EffTailF er) ->
    Effectful u f eh er ~> Effectful u f eh (EffTailF er)
interpretRec i = interpretAllRec $ i . unliftInsIfSingle |+: injectF
{-# INLINE interpretRec #-}

{- |
Using the provided interpretation function, interpret higher-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRecH ::
    forall er ef f u c.
    ( Freer c f
    , HFunctorUnion u
    , HeadHFunctor u (EffHead er)
    , TailHFunctor u (EffTail er)
    , DecompH u er
    ) =>
    HeadMultiToUnionH u er (Effectful u f (EffTail er) ef) ~> Effectful u f (EffTail er) ef ->
    Effectful u f er ef ~> Effectful u f (EffTail er) ef
interpretRecH i = interpretAllRecH $ i |+: injectH
{-# INLINE interpretRecH #-}

-- | Interpret the leading first-order effect class using a delimited continuation.
interpretK ::
    forall er r a f u.
    (MonadFreer f, Union u, DecompF u er) =>
    (a -> Effectful u f LNop (EffTailF er) r) ->
    ( forall x.
      (x -> Effectful u f LNop (EffTailF er) r) ->
      HeadMultiToUnionF u er x ->
      Effectful u f LNop (EffTailF er) r
    ) ->
    Effectful u f LNop er a ->
    Effectful u f LNop (EffTailF er) r
interpretK = toInterpretKFromContT interpretContT
{-# INLINE interpretK #-}

-- | Interpret the leading first-order effect class using a continuation monad transformer.
interpretContT ::
    forall er r f u.
    (MonadFreer f, Union u, DecompF u er) =>
    HeadMultiToUnionF u er ~> ContT r (Effectful u f LNop (EffTailF er)) ->
    Effectful u f LNop er ~> ContT r (Effectful u f LNop (EffTailF er))
interpretContT i =
    interpretContTAll $ i . unliftInsIfSingle |+: lift . injectF
{-# INLINE interpretContT #-}

interpretContTH ::
    forall er ef r f u.
    (MonadFreer f, HFunctorUnion u, DecompH u er, EffTail er ~ LNop, HeadHFunctor u (EffHead er)) =>
    HeadMultiToUnionH u er (ContT r (Effectful u f LNop ef)) ~> ContT r (Effectful u f LNop ef) ->
    Effectful u f er ef ~> ContT r (Effectful u f LNop ef)
interpretContTH i = interpretContTAllH $ i |+: exhaust
{-# INLINE interpretContTH #-}

-- | Interpret the leading first-order effect class using a monad transformer.
interpretT ::
    forall t er f u.
    ( MonadFreer f
    , Union u
    , DecompF u er
    , MonadTrans t
    , Monad (t (Effectful u f LNop (EffTailF er)))
    ) =>
    HeadMultiToUnionF u er ~> t (Effectful u f LNop (EffTailF er)) ->
    Effectful u f LNop er ~> t (Effectful u f LNop (EffTailF er))
interpretT i =
    interpretAll $ i . unliftInsIfSingle |+: lift . injectF
{-# INLINE interpretT #-}

interpose ::
    forall e ef f u c.
    (Freer c f, Union u, MemberF u e ef) =>
    e ~> Effectful u f LNop ef ->
    Effectful u f LNop ef ~> Effectful u f LNop ef
interpose f =
    interpretAllE
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> injectF u

interposeRec ::
    forall e eh ef f u c.
    (Freer c f, Union u, MemberF u e ef, HFunctors u eh) =>
    e ~> Effectful u f eh ef ->
    Effectful u f eh ef ~> Effectful u f eh ef
interposeRec f =
    interpretAllRec
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> injectF u

interposeRecH ::
    forall e eh ef f u c.
    (Freer c f, Union u, MemberH u e eh, HFunctors u eh) =>
    e (Effectful u f eh ef) ~> Effectful u f eh ef ->
    Effectful u f eh ef ~> Effectful u f eh ef
interposeRecH f =
    interpretAllRecH
        \u -> case projectRec u of
            Just e -> f e
            Nothing -> injectH u

interposeK ::
    forall e r a es f u.
    (MonadFreer f, Union u, MemberF u e es) =>
    (a -> Effectful u f LNop es r) ->
    ( forall x.
      (x -> Effectful u f LNop es r) ->
      e x ->
      Effectful u f LNop es r
    ) ->
    Effectful u f LNop es a ->
    Effectful u f LNop es r
interposeK k i = (`runContT` k) . interposeContT \e -> ContT (`i` e)
{-# INLINE interposeK #-}

interposeContT ::
    forall e r es f u.
    ( MonadFreer f
    , Union u
    , MemberF u e es
    ) =>
    e ~> ContT r (Effectful u f LNop es) ->
    Effectful u f LNop es ~> ContT r (Effectful u f LNop es)
interposeContT f =
    interpretContTAll \u -> case projectRec u of
        Just (LiftIns e) -> f e
        Nothing -> lift $ injectF u
{-# INLINE interposeContT #-}

interposeT ::
    forall e t es f u.
    ( MonadFreer f
    , Union u
    , MemberF u e es
    , MonadTrans t
    , Monad (t (Effectful u f LNop es))
    ) =>
    e ~> t (Effectful u f LNop es) ->
    Effectful u f LNop es ~> t (Effectful u f LNop es)
interposeT f =
    interpretAll
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> lift $ injectF u

intercept ::
    forall er f u c.
    (Freer c f, HFunctorUnion u, DecompF u er) =>
    HeadMultiToUnionF u er ~> Effectful u f LNop er ->
    Effectful u f LNop er ~> Effectful u f LNop er
intercept f = interpret f . raiseUnder . decompEff
{-# INLINE intercept #-}

interceptRec ::
    forall er eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u er) =>
    HeadMultiToUnionF u er ~> Effectful u f eh er ->
    Effectful u f eh er ~> Effectful u f eh er
interceptRec f = interpretRec f . raiseUnder . decompEff
{-# INLINE interceptRec #-}

interceptRecH ::
    forall er ef f u c.
    ( Freer c f
    , HFunctorUnion u
    , DecompH u er
    , HeadHFunctor u (EffHead er)
    , TailHFunctor u (EffTail er)
    ) =>
    HeadMultiToUnionH u er (Effectful u f er ef) ~> Effectful u f er ef ->
    Effectful u f er ef ~> Effectful u f er ef
interceptRecH f = interpretRecH f . raiseUnderH . decompEffH
{-# INLINE interceptRecH #-}

interpretAll ::
    forall g ef f u c.
    (Freer c f, Union u, c g) =>
    SumToUnionF u ef ~> g ->
    Effectful u f LNop ef ~> g
interpretAll = interpretAllFH_ exhaust
{-# INLINE interpretAll #-}

interpretAllE ::
    forall eh' ef' ef f u c.
    (Freer c f, Union u) =>
    SumToUnionF u ef ~> Effectful u f eh' ef' ->
    Effectful u f LNop ef ~> Effectful u f eh' ef'
interpretAllE = interpretAllFHE_ exhaust
{-# INLINE interpretAllE #-}

interpretAllH ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Effectful u f eh' ef) ~> Effectful u f eh' ef ->
    Effectful u f eh ef ~> Effectful u f eh' ef
interpretAllH fh =
    interpretAllH_ $ fh . hfmap (interpretAllH @_ @eh fh . Effectful)
{-# INLINE interpretAllH #-}

interpretAllH_ ::
    forall eh' eh ef f u c.
    (Freer c f, Union u) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh ef)) ~> Effectful u f eh' ef ->
    Effectful u f eh ef ~> Effectful u f eh' ef
interpretAllH_ fh = interpretAllFHE_ fh injectF
{-# INLINE interpretAllH_ #-}

interpretAllFH ::
    forall g eh ef f u c.
    (Freer c f, Union u, c g, HFunctors u eh) =>
    SumToUnionH u eh g ~> g ->
    SumToUnionF u ef ~> g ->
    Effectful u f eh ef ~> g
interpretAllFH fh ff =
    interpretAllFH_ (fh . hfmap (interpretAllFH @_ @eh @ef fh ff . Effectful)) ff
{-# INLINE interpretAllFH #-}

interpretAllFH_ ::
    forall g eh ef f u c.
    (Freer c f, Union u, c g) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh ef)) ~> g ->
    SumToUnionF u ef ~> g ->
    Effectful u f eh ef ~> g
interpretAllFH_ fh ff =
    interpretFreer (caseH fh (ff . unliftIns)) . unliftEff

interpretAllFHE ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Effectful u f eh' ef') ~> Effectful u f eh' ef' ->
    SumToUnionF u ef ~> Effectful u f eh' ef' ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
interpretAllFHE fh ff =
    interpretAllFHE_ (fh . hfmap (interpretAllFHE @_ @_ @eh @ef fh ff . Effectful)) ff
{-# INLINE interpretAllFHE #-}

interpretAllFHE_ ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh ef)) ~> Effectful u f eh' ef' ->
    SumToUnionF u ef ~> Effectful u f eh' ef' ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
interpretAllFHE_ fh ff =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseH fh (ff . unliftIns)
            >>> unEffectful
            >>> unHefty

interpretAllRec ::
    forall ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionF u ef ~> Effectful u f eh ef' ->
    Effectful u f eh ef ~> Effectful u f eh ef'
interpretAllRec = interpretAllFHE injectH
{-# INLINE interpretAllRec #-}

interpretAllRecH ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Effectful u f eh' ef) ~> Effectful u f eh' ef ->
    Effectful u f eh ef ~> Effectful u f eh' ef
interpretAllRecH fh = interpretAllFHE fh injectF
{-# INLINE interpretAllRecH #-}

interpretKAll ::
    forall r a ef f u.
    (MonadFreer f, Union u) =>
    (a -> Effectful u f LNop ef r) ->
    (forall x. (x -> Effectful u f LNop ef r) -> SumToUnionF u ef x -> Effectful u f LNop ef r) ->
    Effectful u f LNop ef a ->
    Effectful u f LNop ef r
interpretKAll = toInterpretKFromContT interpretContTAll
{-# INLINE interpretKAll #-}

interpretKAllH ::
    forall eh' r a eh ef f u.
    (MonadFreer f, Union u, HFunctors u eh) =>
    (a -> Effectful u f eh' ef r) ->
    ( forall x.
      (x -> Effectful u f eh' ef r) ->
      SumToUnionH u eh (ContT r (Effectful u f eh' ef)) x ->
      Effectful u f eh' ef r
    ) ->
    Effectful u f eh ef a ->
    Effectful u f eh' ef r
interpretKAllH = toInterpretKFromContT interpretContTAllH
{-# INLINE interpretKAllH #-}

interpretKAllFH ::
    forall g r a eh ef f u.
    (MonadFreer f, Union u, HFunctors u eh) =>
    (a -> g r) ->
    (forall x. (x -> g r) -> SumToUnionH u eh (ContT r g) x -> g r) ->
    (forall x. (x -> g r) -> SumToUnionF u ef x -> g r) ->
    Effectful u f eh ef a ->
    g r
interpretKAllFH = toInterpretKFromContT2 interpretContTAllFH
{-# INLINE interpretKAllFH #-}

interpretContTAll ::
    forall g r ef f u.
    (MonadFreer f, Union u) =>
    SumToUnionF u ef ~> ContT r g ->
    Effectful u f LNop ef ~> ContT r g
interpretContTAll f =
    transCont
        . interpretFreerK (caseH exhaust (detransContT . f . unliftIns))
        . unliftEff

interpretContTAllH ::
    forall eh' r eh ef f u.
    (MonadFreer f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (ContT r (Effectful u f eh' ef)) ~> ContT r (Effectful u f eh' ef) ->
    Effectful u f eh ef ~> ContT r (Effectful u f eh' ef)
interpretContTAllH fh = interpretContTAllFH fh (lift . injectF)
{-# INLINE interpretContTAllH #-}

interpretContTAllFH ::
    forall g r eh ef f u.
    (MonadFreer f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (ContT r g) ~> ContT r g ->
    SumToUnionF u ef ~> ContT r g ->
    Effectful u f eh ef ~> ContT r g
interpretContTAllFH fh ff =
    transCont
        . interpretFreerK
            ( detransContT
                . caseH
                    (fh . hfmap (interpretContTAllFH @_ @_ @eh @ef fh ff . Effectful))
                    (ff . unliftIns)
            )
        . unliftEff

transCont :: Cont (m r) ~> ContT r m
transCont (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE transCont #-}

detransContT :: ContT r m ~> Cont (m r)
detransContT (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE detransContT #-}

toInterpretKFromContT ::
    ((e ~> ContT r m) -> f ~> ContT r m') ->
    (a -> m' r) ->
    (forall x. (x -> m r) -> e x -> m r) ->
    f a ->
    m' r
toInterpretKFromContT intContT k i = (`runContT` k) . intContT \e -> ContT (`i` e)
{-# INLINE toInterpretKFromContT #-}

toInterpretKFromContT2 ::
    ((e1 ~> ContT r m) -> (e2 ~> ContT r m) -> f ~> ContT r m') ->
    (a -> m' r) ->
    (forall x. (x -> m r) -> e1 x -> m r) ->
    (forall x. (x -> m r) -> e2 x -> m r) ->
    f a ->
    m' r
toInterpretKFromContT2 intContT k i1 i2 =
    (`runContT` k) . intContT (\e -> ContT (`i1` e)) (\e -> ContT (`i2` e))
{-# INLINE toInterpretKFromContT2 #-}

interpretTAll ::
    forall t g ef f u.
    (MonadFreer f, Union u, Monad (t g)) =>
    SumToUnionF u ef ~> t g ->
    Effectful u f LNop ef ~> t g
interpretTAll = interpretAll
{-# INLINE interpretTAll #-}

interpretTAllH ::
    forall eh' t eh ef f u.
    (MonadFreer f, Union u, MonadTrans t, Monad (t (Effectful u f eh' ef)), HFunctors u eh) =>
    SumToUnionH u eh (t (Effectful u f eh' ef)) ~> t (Effectful u f eh' ef) ->
    Effectful u f eh ef ~> t (Effectful u f eh' ef)
interpretTAllH i = interpretTAllFH i (lift . injectF)
{-# INLINE interpretTAllH #-}

interpretTAllFH ::
    forall g t eh ef f u.
    (MonadFreer f, Union u, MonadTrans t, Monad (t g), HFunctors u eh) =>
    SumToUnionH u eh (t g) ~> t g ->
    SumToUnionF u ef ~> t g ->
    Effectful u f eh ef ~> t g
interpretTAllFH fh ff =
    interpretAllFH_ (fh . hfmap (interpretTAllFH @_ @_ @eh @ef fh ff . Effectful)) ff
{-# INLINE interpretTAllFH #-}

transform ::
    forall e1r e2r eh f u c.
    (Freer c f, Union u, HFunctors u eh, DecompF u e1r, DecompF u e2r, EffTailF e1r ~ EffTailF e2r) =>
    (HeadMultiToUnionF u e1r ~> HeadMultiToUnionF u e2r) ->
    Effectful u f eh e1r ~> Effectful u f eh e2r
transform f = transformAll $ inject0 . liftInsIfSingle . f . unliftInsIfSingle |+: weaken
{-# INLINE transform #-}

transformH ::
    forall e1r e2r ef f u c.
    ( Freer c f
    , Union u
    , DecompH u e1r
    , DecompH u e2r
    , EffTail e1r ~ EffTail e2r
    , HFunctors u e1r
    , HFunctors u e2r
    ) =>
    ( HeadMultiToUnionH u e1r (Effectful u f e2r ef)
        ~> HeadMultiToUnionH u e2r (Effectful u f e2r ef)
    ) ->
    Effectful u f e1r ef ~> Effectful u f e2r ef
transformH f = transformAllH $ inject0 . f |+: weaken
{-# INLINE transformH #-}

transformH_ ::
    forall e1r e2r ef f u c.
    ( Freer c f
    , Union u
    , DecompH u e1r
    , DecompH u e2r
    , EffTail e1r ~ EffTail e2r
    , HFunctors u e1r
    ) =>
    ( HeadMultiToUnionH u e1r (Hefty f (EffUnion u e2r ef))
        ~> HeadMultiToUnionH u e2r (Hefty f (EffUnion u e2r ef))
    ) ->
    Effectful u f e1r ef ~> Effectful u f e2r ef
transformH_ f = transformAllH_ $ inject0 . f |+: weaken
{-# INLINE transformH_ #-}

transformFH ::
    forall e1rh e2rh e1rf e2rf f u c.
    ( Freer c f
    , Union u
    , DecompF u e1rf
    , DecompF u e2rf
    , EffTailF e1rf ~ EffTailF e2rf
    , DecompH u e1rh
    , DecompH u e2rh
    , EffTail e1rh ~ EffTail e2rh
    , HFunctors u e1rh
    , HFunctors u e2rh
    ) =>
    ( HeadMultiToUnionH u e1rh (Effectful u f e2rh e2rf)
        ~> HeadMultiToUnionH u e2rh (Effectful u f e2rh e2rf)
    ) ->
    (HeadMultiToUnionF u e1rf ~> HeadMultiToUnionF u e2rf) ->
    Effectful u f e1rh e1rf ~> Effectful u f e2rh e2rf
transformFH fh ff =
    transformAllFH
        (inject0 . fh |+: weaken)
        (inject0 . (liftInsIfSingle . ff . unliftInsIfSingle) |+: weaken)
{-# INLINE transformFH #-}

transformFH_ ::
    forall e1rh e2rh e1rf e2rf f u c.
    ( Freer c f
    , Union u
    , DecompF u e1rf
    , DecompF u e2rf
    , EffTailF e1rf ~ EffTailF e2rf
    , DecompH u e1rh
    , DecompH u e2rh
    , EffTail e1rh ~ EffTail e2rh
    , HFunctors u e1rh
    ) =>
    ( HeadMultiToUnionH u e1rh (Hefty f (EffUnion u e2rh e2rf))
        ~> HeadMultiToUnionH u e2rh (Hefty f (EffUnion u e2rh e2rf))
    ) ->
    (HeadMultiToUnionF u e1rf ~> HeadMultiToUnionF u e2rf) ->
    Effectful u f e1rh e1rf ~> Effectful u f e2rh e2rf
transformFH_ fh ff =
    transformAllFH_
        (inject0 . fh |+: weaken)
        (inject0 . (liftInsIfSingle . ff . unliftInsIfSingle) |+: weaken)
{-# INLINE transformFH_ #-}

transformAll ::
    forall ef' ef eh f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionF u ef ~> SumToUnionF u ef' ->
    Effectful u f eh ef ~> Effectful u f eh ef'
transformAll = transformAllFH_ id
{-# INLINE transformAll #-}

transformAllH ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh, HFunctors u eh') =>
    SumToUnionH u eh (Effectful u f eh' ef)
        ~> SumToUnionH u eh' (Effectful u f eh' ef) ->
    Effectful u f eh ef ~> Effectful u f eh' ef
transformAllH f = transformAllH_ (hfmap unEffectful . f . hfmap Effectful)
{-# INLINE transformAllH #-}

transformAllH_ ::
    forall eh' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh' ef))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef)) ->
    Effectful u f eh ef ~> Effectful u f eh' ef
transformAllH_ f = transformAllFH_ f id
{-# INLINE transformAllH_ #-}

transformAllFH ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh, HFunctors u eh') =>
    SumToUnionH u eh (Effectful u f eh' ef')
        ~> SumToUnionH u eh' (Effectful u f eh' ef') ->
    (SumToUnionF u ef ~> SumToUnionF u ef') ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
transformAllFH fh = transformAllFH_ (hfmap unEffectful . fh . hfmap Effectful)
{-# INLINE transformAllFH #-}

transformAllFH_ ::
    forall eh' ef' eh ef f u c.
    (Freer c f, Union u, HFunctors u eh) =>
    SumToUnionH u eh (Hefty f (EffUnion u eh' ef'))
        ~> SumToUnionH u eh' (Hefty f (EffUnion u eh' ef')) ->
    (SumToUnionF u ef ~> SumToUnionF u ef') ->
    Effectful u f eh ef ~> Effectful u f eh' ef'
transformAllFH_ fh ff =
    overEffectful
        . overHefty
        $ transformFreer
        $ caseH
            (Inl . fh . hfmap (unEffectful . transformAllFH_ @eh' @ef' @eh @ef fh ff . Effectful))
            (Inr . LiftIns . ff . unliftIns)

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
raiseH = transformAllH_ weaken
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
raiseUnderH = transformAllH_ weakenUnder
{-# INLINE raiseUnderH #-}

splitEff ::
    forall f' er f u c.
    (Freer c f', Freer c f, Union u, DecompF u er) =>
    Effectful u f LNop er ~> f' (HeadMultiToUnionF u er + Effectful u f LNop (EffTailF er))
splitEff = interpretAll $ liftIns . (L1 . unliftInsIfSingle |+: R1 . injectF)
{-# INLINE splitEff #-}

send0 ::
    ( Freer c f
    , Union u
    , DecompF u er
    , NormalizeSig (LiftIns (EffHeadF er)) ~ SingleSig (LiftIns (EffHeadF er))
    ) =>
    EffHeadF er ~> Effectful u f eh er
send0 = liftFreer . liftIns . Inr . LiftIns . inject0 . LiftIns

injectH ::
    (Freer c f, HFunctors u eh) =>
    SumToUnionH u eh (Effectful u f eh ef) ~> Effectful u f eh ef
injectH = liftFreer . liftIns . Inl . hfmap unEffectful
{-# INLINE injectH #-}

injectF :: Freer c f => SumToUnionF u ef ~> Effectful u f eh ef
injectF = liftFreer . liftIns . Inr . LiftIns
{-# INLINE injectF #-}

liftFreer :: Freer c f => f (EffUnion u eh ef (Hefty f (EffUnion u eh ef))) ~> Effectful u f eh ef
liftFreer = coerce
{-# INLINE liftFreer #-}

unliftEff :: Freer c f => Effectful u f eh ef ~> f (EffUnion u eh ef (Hefty f (EffUnion u eh ef)))
unliftEff = coerce
{-# INLINE unliftEff #-}
