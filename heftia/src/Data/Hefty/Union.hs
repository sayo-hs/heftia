{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A type class representing a general open union for higher-order effects, independent of the internal
implementation.
-}
module Data.Hefty.Union where

import Control.Effect (type (~>))
import Control.Monad ((<=<))
import Data.Bool.Singletons (SBool (SFalse, STrue))
import Data.Effect (LNop, LiftIns (LiftIns), Nop, SigClass, unliftIns)
import Data.Effect.HFunctor (HFunctor, caseH, (:+:) (Inl, Inr))
import Data.Free.Sum (type (+))
import Data.Kind (Constraint)
import Data.Singletons (SingI, sing, Sing)
import Data.Singletons.TH (singletons)
import Data.Type.Bool (If, type (||), Not, type (&&))
import Data.Type.Equality ((:~:) (Refl), type (==))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError, Nat)
import Data.Constraint (Dict (Dict))
import qualified GHC.TypeNats as N

{- |
A type class representing a general open union for higher-order effects, independent of the internal
implementation.
-}
class Union (u :: [SigClass] -> SigClass) where
    {-# MINIMAL inject, project, exhaust, (comp | (inject0, weaken), decomp | (|+:)) #-}

    type HasMembership u (e :: SigClass) (es :: [SigClass]) :: Constraint

    inject :: HasMembership u e es => e f ~> u es f
    project :: HasMembership u e es => u es f a -> Maybe (e f a)

    exhaust :: u '[] f a -> x

    comp :: Either (e f a) (u es f a) -> u (e ': es) f a
    comp = \case
        Left x -> inject0 x
        Right x -> weaken x
    {-# INLINE comp #-}

    decomp :: u (e ': es) f a -> (e :+: u es) f a
    decomp = Inl |+: Inr
    {-# INLINE decomp #-}

    infixr 5 |+:
    (|+:) :: (e f a -> r) -> (u es f a -> r) -> u (e ': es) f a -> r
    f |+: g = caseH f g . decomp
    {-# INLINE (|+:) #-}

    inject0 :: e f ~> u (e ': es) f
    inject0 = comp . Left
    {-# INLINE inject0 #-}

    injectUnder :: h2 f ~> u (h1 ': h2 ': es) f
    injectUnder = weaken . inject0
    {-# INLINE injectUnder #-}

    injectUnder2 :: h3 f ~> u (h1 ': h2 ': h3 ': es) f
    injectUnder2 = weaken2 . inject0
    {-# INLINE injectUnder2 #-}

    injectUnder3 :: h4 f ~> u (h1 ': h2 ': h3 ': h4 ': es) f
    injectUnder3 = weaken3 . inject0
    {-# INLINE injectUnder3 #-}

    weaken :: u es f ~> u (e ': es) f
    weaken = comp . Right
    {-# INLINE weaken #-}

    weaken2 :: u es f ~> u (e1 ': e2 ': es) f
    weaken2 = weaken . weaken
    {-# INLINE weaken2 #-}

    weaken3 :: u es f ~> u (e1 ': e2 ': e3 ': es) f
    weaken3 = weaken2 . weaken
    {-# INLINE weaken3 #-}

    weaken4 :: u es f ~> u (e1 ': e2 ': e3 ': e4 ': es) f
    weaken4 = weaken3 . weaken
    {-# INLINE weaken4 #-}

    weakenUnder :: u (e1 ': es) f ~> u (e1 ': e2 ': es) f
    weakenUnder = inject0 |+: weaken2

    weakenUnder2 :: u (e1 ': e2 ': es) f ~> u (e1 ': e2 ': e3 ': es) f
    weakenUnder2 = inject0 |+: injectUnder |+: weaken3

    weakenUnder3 :: u (e1 ': e2 ': e3 ': es) f ~> u (e1 ': e2 ': e3 ': e4 ': es) f
    weakenUnder3 = inject0 |+: injectUnder |+: injectUnder2 |+: weaken4

    weaken2Under :: u (e1 ': es) f ~> u (e1 ': e2 ': e3 ': es) f
    weaken2Under = inject0 |+: weaken3

    weaken2Under2 :: u (e1 ': e2 ': es) f ~> u (e1 ': e2 ': e3 ': e4 ': es) f
    weaken2Under2 = inject0 |+: injectUnder |+: weaken4

    weaken3Under :: u (e1 ': es) f ~> u (e1 ': e2 ': e3 ': e4 ': es) f
    weaken3Under = inject0 |+: weaken4

    flipUnion :: u (e1 ': e2 ': es) f ~> u (e2 ': e1 ': es) f
    flipUnion = injectUnder |+: inject0 |+: weaken2

    flipUnion3 :: u (e1 ': e2 ': e3 ': es) f ~> u (e3 ': e2 ': e1 ': es) f
    flipUnion3 = injectUnder2 |+: injectUnder |+: inject0 |+: weaken3

    flipUnionUnder :: u (e1 ': e2 ': e3 ': es) f ~> u (e1 ': e3 ': e2 ': es) f
    flipUnionUnder = inject0 |+: injectUnder2 |+: injectUnder |+: weaken3

    rot3 :: u (e1 ': e2 ': e3 ': es) f ~> u (e2 ': e3 ': e1 ': es) f
    rot3 = injectUnder2 |+: inject0 |+: injectUnder |+: weaken3

    rot3' :: u (e1 ': e2 ': e3 ': es) f ~> u (e3 ': e1 ': e2 ': es) f
    rot3' = injectUnder |+: injectUnder2 |+: inject0 |+: weaken3

    bundleUnion2 :: u (e1 ': e2 ': es) f ~> u (u '[e1, e2] ': es) f
    bundleUnion2 = inject0 . inject0 |+: inject0 . injectUnder |+: weaken

    bundleUnion3 :: u (e1 ': e2 ': e3 ': es) f ~> u (u '[e1, e2, e3] ': es) f
    bundleUnion3 =
        (inject0 . inject0)
            |+: (inject0 . injectUnder)
            |+: (inject0 . injectUnder2)
            |+: weaken

    bundleUnion4 ::
        u (e1 ': e2 ': e3 ': e4 ': es) f ~> u (u '[e1, e2, e3, e4] ': es) f
    bundleUnion4 =
        (inject0 . inject0)
            |+: (inject0 . injectUnder)
            |+: (inject0 . injectUnder2)
            |+: (inject0 . injectUnder3)
            |+: weaken

    unbundleUnion2 :: u (u '[e1, e2] ': es) f ~> u (e1 ': e2 ': es) f
    unbundleUnion2 = (inject0 |+: injectUnder |+: exhaust) |+: weaken2

    unbundleUnion3 :: u (u '[e1, e2, e3] ': es) f ~> u (e1 ': e2 ': e3 ': es) f
    unbundleUnion3 = (inject0 |+: injectUnder |+: injectUnder2 |+: exhaust) |+: weaken3

    unbundleUnion4 ::
        u (u '[e1, e2, e3, e4] ': es) f
            ~> u (e1 ': e2 ': e3 ': e4 ': es) f
    unbundleUnion4 =
        (inject0 |+: injectUnder |+: injectUnder2 |+: injectUnder3 |+: exhaust)
            |+: weaken4

$(singletons [d|
    data Found = FoundIn FoundLevel | NotFound
    data FoundLevel = CurrentLevel | LowerLevel
    |])

type HFunctorUnion u = HFunctorUnion_ (ForallHFunctor u) u

-- A hack to avoid the "Quantified predicate must have a class or type variable head" error.
class
    ( Union u
    , forall e es. (HFunctor e, forallHFunctor es) => forallHFunctor (e ': es)
    , forall es. forallHFunctor es => HFunctor (u es)
    , forallHFunctor ~ ForallHFunctor u
    , forallHFunctor '[]
    ) =>
    HFunctorUnion_ forallHFunctor u
        | u -> forallHFunctor
    where
    type ForallHFunctor u :: [SigClass] -> Constraint

class MemberRec u e es => MemberRec' u e es where
    member ::
        ( forall lvl foundInHeadUnion.
            ( SearchMemberRec es u e es ('FoundIn lvl) foundInHeadUnion
            , SingI lvl
            , HasMembership u e es
            , SingI foundInHeadUnion
            ) => r
        ) -> r

instance
    ( SearchMemberRec es u e es found fihu
    , MemberFound e es found
    , SingI fihu
    , found ~ 'FoundIn lvl
    , SingI lvl
    , HasMembership u e es
    ) =>
    MemberRec' u e es where
    member x = withFound @e @es @found x
    {-# INLINE member #-}

{-
type family DetermineLevel u e es :: FoundLevel where
    DetermineLevel u e (e ': _) = 'CurrentLevel
    DetermineLevel u e (u es ': r) = If (IsMemberRec u e es) 'LowerLevel (DetermineLevel u e r)
    DetermineLevel u e (_ ': es) = DetermineLevel u e es

type family IsMemberRec u e es :: Bool where
    IsMemberRec u e (e ': _) = 'True
    IsMemberRec u e (u es ': r) = IsMemberRec u e es || IsMemberRec u e r
    IsMemberRec u e (_ ': es) = IsMemberRec u e es
    IsMemberRec _ _ _ = 'False
-}

class MemberRec (u :: [SigClass] -> SigClass) e es where
    injectRec :: e f ~> u es f
    projectRec :: u es f a -> Maybe (e f a)

instance
    (SearchMemberRec es u e es found fihu, MemberFound e es found, SingI fihu) =>
    MemberRec u e es
    where
    injectRec = withFound @e @es @found $ injectSMR @es Refl sing sing
    projectRec = withFound @e @es @found $ projectSMR @es Refl sing sing
    {-# INLINE injectRec #-}
    {-# INLINE projectRec #-}

class MemberFound e es found where
    withFound :: (forall lvl. (found ~ 'FoundIn lvl, SingI lvl) => a) -> a

instance SingI lvl => MemberFound e es ('FoundIn lvl) where
    withFound a = a
    {-# INLINE withFound #-}

-- A stopgap until upgrading to base-4.19.
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-TypeError.html#t:Unsatisfiable
instance
    TypeError
        ( 'Text "The effect class: " ':<>: 'ShowType e
            ':$$: 'Text " was not found in the union:"
            ':$$: 'Text "    " ':<>: 'ShowType es
        ) =>
    MemberFound e es 'NotFound
    where
    withFound _ = error "unreachable"

type SearchMemberRec rest u e = SearchMemberRec_ (NextSearchMemberRecAction rest u e) rest u e

class
    SearchMemberRec_
        (act :: SearchMemberRecAction)
        (rest :: [SigClass])
        (u :: [SigClass] -> SigClass)
        (e :: SigClass)
        (es :: [SigClass])
        (found :: Found)
        (foundInHeadUnion :: Found)
        | rest e -> found foundInHeadUnion
    where
    injectSMR_ :: found :~: 'FoundIn lvl -> SFound ('FoundIn lvl) -> SFound foundInHeadUnion -> e f ~> u es f
    projectSMR_ :: found :~: 'FoundIn lvl -> SFound ('FoundIn lvl) -> SFound foundInHeadUnion -> u es f a -> Maybe (e f a)

injectSMR ::
    forall rest u e es found lvl foundInHeadUnion f.
    SearchMemberRec rest u e es found foundInHeadUnion =>
    found :~: 'FoundIn lvl ->
    SFound ('FoundIn lvl) ->
    SFound foundInHeadUnion ->
    e f ~> u es f
injectSMR = injectSMR_ @(NextSearchMemberRecAction rest u e) @rest
{-# INLINE injectSMR #-}

projectSMR ::
    forall rest u e es found lvl foundInHeadUnion f a.
    SearchMemberRec rest u e es found foundInHeadUnion =>
    found :~: 'FoundIn lvl ->
    SFound ('FoundIn lvl) ->
    SFound foundInHeadUnion ->
    u es f a ->
    Maybe (e f a)
projectSMR = projectSMR_ @(NextSearchMemberRecAction rest u e) @rest
{-# INLINE projectSMR #-}

data SearchMemberRecAction = SmrStop | SmrRight | SmrDown

type family NextSearchMemberRecAction rest (u :: [SigClass] -> SigClass) e where
    NextSearchMemberRecAction (e ': _) u e = 'SmrStop
    NextSearchMemberRecAction (u _ ': _) u e = 'SmrDown
    NextSearchMemberRecAction _ _ _ = 'SmrRight

instance
    (HasMembership u e es, Union u) =>
    SearchMemberRec_ 'SmrStop (e ': _tail) u e es ('FoundIn 'CurrentLevel) 'NotFound
    where
    injectSMR_ _ _ _ = inject
    projectSMR_ _ _ _ = project
    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

type family IsFound found where
    IsFound ('FoundIn _) = 'True
    IsFound 'NotFound = 'False

instance
    ( SearchMemberRec es' u e es' foundInHead fihuHead
    , isFoundInHead ~ IsFound foundInHead
    , If isFoundInHead (HasMembership u (u es') es) (() :: Constraint)
    , SearchMemberRec (If isFoundInHead '[] tail) u e es foundInTail fihuTail
    , found ~ If isFoundInHead ('FoundIn 'LowerLevel) foundInTail
    , Union u
    , SingI fihuHead
    , SingI fihuTail
    ) =>
    SearchMemberRec_ 'SmrDown (u es' ': tail) u e es found foundInHead
    where
    injectSMR_ Refl found = \case
        SFoundIn lvl -> inject . injectSMR @es' @u @_ @es' Refl (SFoundIn lvl) sing
        SNotFound -> injectSMR @tail Refl found sing

    projectSMR_ Refl found = \case
        SFoundIn lvl -> projectSMR @es' @u @_ @es' Refl (SFoundIn lvl) sing <=< project
        SNotFound -> projectSMR @tail Refl found sing

    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

instance
    ( SearchMemberRec rest u e rest found fihu
    , HasMembership u e (_e ': rest)
    , SingI fihu
    , Union u
    ) =>
    SearchMemberRec_ 'SmrRight (_e ': rest) u e (_e ': rest) found 'NotFound
    where
    injectSMR_ Refl (SFoundIn lvl) _ = case lvl of
        SCurrentLevel -> inject
        SLowerLevel -> weaken . injectSMR @rest Refl sing sing

    projectSMR_ Refl (SFoundIn lvl) _ = case lvl of
        SCurrentLevel -> project
        SLowerLevel -> const Nothing |+: projectSMR @rest Refl sing sing

    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

instance SearchMemberRec_ act '[] u e es 'NotFound 'NotFound where
    injectSMR_ = \case {}
    projectSMR_ = \case {}
    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

infixr 5 |+
(|+) :: Union u => (e a -> r) -> (u es f a -> r) -> u (LiftIns e ': es) f a -> r
f |+ g = f . unliftIns |+: g
{-# INLINE (|+) #-}

{- |
Recursively decompose the sum of first-order effects into a list, following the direction of right
association, with normalization.
-}
type U u ef = UH u (LiftIns ef)

{- |
Recursively decompose the sum of higher-order effects into a list, following the direction of right
association, with normalization.
-}
type UH u eh = SumToUnionList u (NormalizeSig eh)

{- |
Recursively decompose the sum of higher-order effects into a list, following the direction of right
association.
-}
type family SumToUnionList (u :: [SigClass] -> SigClass) (e :: SigClass) :: [SigClass] where
    SumToUnionList u (e1 :+: e2) = MultiListToUnion u (SumToUnionList u e1) ': SumToUnionList u e2
    SumToUnionList u LNop = '[]
    SumToUnionList u (SingleSig e) = '[e]

{- |
Convert a given list of higher-order effect classes into a suitable representation type for each
case of being empty, single, or multiple.
-}
type family MultiListToUnion (u :: [SigClass] -> SigClass) (es :: [SigClass]) :: SigClass where
    MultiListToUnion u '[] = LNop
    MultiListToUnion u '[e] = e
    MultiListToUnion u es = u es

{- |
Normalization in preparation for decomposing the sum of effect classes into a list.

In particular, mark an indivisible, single effect class by applying the t'SingleSig' wrapper to it.
-}
type family NormalizeSig e where
    NormalizeSig LNop = LNop
    NormalizeSig (LiftIns (e1 + e2)) = NormalizeSig (LiftIns e1) :+: NormalizeSig (LiftIns e2)
    NormalizeSig (e1 :+: e2) = NormalizeSig e1 :+: NormalizeSig e2
    NormalizeSig e = SingleSig e

{- |
A wrapper to mark a single, i.e., a higher-order effect class that cannot be further decomposed as
a sum.
-}
newtype SingleSig (e :: SigClass) f a = SingleSig {unSingleSig :: e f a}
    deriving newtype (HFunctor)

type family UnionListToSum (u :: [SigClass] -> SigClass) (es :: [SigClass]) :: SigClass where
    UnionListToSum u '[e] = UnionToSum u e
    UnionListToSum u '[] = LNop
    UnionListToSum u (e ': r) = UnionToSum u e :+: UnionListToSum u r

type family UnionToSum (u :: [SigClass] -> SigClass) (e :: SigClass) :: SigClass where
    UnionToSum u (u es) = UnionListToSum u es
    UnionToSum u e = e

type S u es = UnionListToSum u es Nop
type SH u es = UnionListToSum u es

type NormalFormUnionList u es = U u (S u es) ~ es
type NormalFormUnionListH u es = UH u (SH u es) ~ es

type NFU u es = NormalFormUnionList u es
type NFUH u es = NormalFormUnionListH u es

type HeadIns le = LiftInsIfSingle (UnliftIfSingle le) le

type family UnliftIfSingle e where
    UnliftIfSingle (LiftIns e) = e
    UnliftIfSingle e = e Nop

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

type family TypeIndex (xs :: [k]) (x :: k) :: Nat where
    TypeIndex (x ': xs) x = 0
    TypeIndex (y ': xs) x = 1 N.+ TypeIndex xs x
    TypeIndex '[] x =
        TypeError
            ('Text "The effect class " ':<>: 'ShowType x ':<>: 'Text " was not found in the list.")
