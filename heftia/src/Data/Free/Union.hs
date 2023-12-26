{-# LANGUAGE AllowAmbiguousTypes #-}
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

A type class representing a general open union for first-order effects, independent of the internal
implementation.
-}
module Data.Free.Union where

import Control.Effect.Class (Instruction, type (~>))
import Control.Freer (InsClass)
import Control.Monad ((<=<))
import Data.Bool.Singletons (SBool (SFalse, STrue))
import Data.Kind (Constraint)
import Data.Singletons (SingI, sing)
import Data.Type.Bool (If, type (||))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

{- |
A type class representing a general open union for first-order effects, independent of the internal
implementation.
-}
class Union (u :: [Instruction] -> Instruction) where
    {-# MINIMAL inject, project, absurdUnion, (comp | (inject0, weaken), decomp | (+:)) #-}

    type HasMembership u (f :: Instruction) (fs :: [Instruction]) :: Constraint

    inject :: HasMembership u f fs => f ~> u fs
    project :: HasMembership u f fs => u fs a -> Maybe (f a)

    absurdUnion :: u '[] a -> x

    comp :: Either (f a) (u fs a) -> u (f ': fs) a
    comp = \case
        Left x -> inject0 x
        Right x -> weaken x
    {-# INLINE comp #-}

    decomp :: u (f ': fs) a -> Either (f a) (u fs a)
    decomp = Left +: Right
    {-# INLINE decomp #-}

    infixr 5 +:
    (+:) :: (f a -> r) -> (u fs a -> r) -> u (f ': fs) a -> r
    (f +: g) u = case decomp u of
        Left x -> f x
        Right x -> g x
    {-# INLINE (+:) #-}

    inject0 :: f ~> u (f ': fs)
    inject0 = comp . Left
    {-# INLINE inject0 #-}

    injectUnder :: f2 ~> u (f1 ': f2 ': fs)
    injectUnder = weaken . inject0
    {-# INLINE injectUnder #-}

    injectUnder2 :: f3 ~> u (f1 ': f2 ': f3 ': fs)
    injectUnder2 = weaken2 . inject0
    {-# INLINE injectUnder2 #-}

    injectUnder3 :: f4 ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    injectUnder3 = weaken3 . inject0
    {-# INLINE injectUnder3 #-}

    weaken :: u fs a -> u (f ': fs) a
    weaken = comp . Right
    {-# INLINE weaken #-}

    weaken2 :: u fs a -> u (f1 ': f2 ': fs) a
    weaken2 = weaken . weaken
    {-# INLINE weaken2 #-}

    weaken3 :: u fs a -> u (f1 ': f2 ': f3 ': fs) a
    weaken3 = weaken2 . weaken
    {-# INLINE weaken3 #-}

    weaken4 :: u fs a -> u (f1 ': f2 ': f3 ': f4 ': fs) a
    weaken4 = weaken3 . weaken
    {-# INLINE weaken4 #-}

    weakenUnder :: u (f1 ': fs) ~> u (f1 ': f2 ': fs)
    weakenUnder = inject0 +: weaken2

    weakenUnder2 :: u (f1 ': f2 ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    weakenUnder2 = inject0 +: injectUnder +: weaken3

    weakenUnder3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weakenUnder3 = inject0 +: injectUnder +: injectUnder2 +: weaken4

    weaken2Under :: u (f1 ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    weaken2Under = inject0 +: weaken3

    weaken2Under2 :: u (f1 ': f2 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weaken2Under2 = inject0 +: injectUnder +: weaken4

    weaken3Under :: u (f1 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weaken3Under = inject0 +: weaken4

    flipUnion :: u (f1 ': f2 ': fs) ~> u (f2 ': f1 ': fs)
    flipUnion = injectUnder +: inject0 +: weaken2

    flipUnion3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f3 ': f2 ': f1 ': fs)
    flipUnion3 = injectUnder2 +: injectUnder +: inject0 +: weaken3

    flipUnionUnder :: u (f1 ': f2 ': f3 ': fs) ~> u (f1 ': f3 ': f2 ': fs)
    flipUnionUnder = inject0 +: injectUnder2 +: injectUnder +: weaken3

    rot3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f2 ': f3 ': f1 ': fs)
    rot3 = injectUnder2 +: inject0 +: injectUnder +: weaken3

    rot3' :: u (f1 ': f2 ': f3 ': fs) ~> u (f3 ': f1 ': f2 ': fs)
    rot3' = injectUnder +: injectUnder2 +: inject0 +: weaken3

    bundleUnion2 :: Union u' => u (f1 ': f2 ': fs) ~> u (u' '[f1, f2] ': fs)
    bundleUnion2 = inject0 . inject0 +: inject0 . injectUnder +: weaken

    bundleUnion3 :: Union u' => u (f1 ': f2 ': f3 ': fs) ~> u (u' '[f1, f2, f3] ': fs)
    bundleUnion3 =
        (inject0 . inject0)
            +: (inject0 . injectUnder)
            +: (inject0 . injectUnder2)
            +: weaken

    bundleUnion4 :: Union u' => u (f1 ': f2 ': f3 ': f4 ': fs) ~> u (u' '[f1, f2, f3, f4] ': fs)
    bundleUnion4 =
        (inject0 . inject0)
            +: (inject0 . injectUnder)
            +: (inject0 . injectUnder2)
            +: (inject0 . injectUnder3)
            +: weaken

    unbundleUnion2 :: Union u' => u (u' '[f1, f2] ': fs) ~> u (f1 ': f2 ': fs)
    unbundleUnion2 = (inject0 +: injectUnder +: absurdUnion) +: weaken2

    unbundleUnion3 :: Union u' => u (u' '[f1, f2, f3] ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    unbundleUnion3 = (inject0 +: injectUnder +: injectUnder2 +: absurdUnion) +: weaken3

    unbundleUnion4 :: Union u' => u (u' '[f1, f2, f3, f4] ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    unbundleUnion4 =
        (inject0 +: injectUnder +: injectUnder2 +: injectUnder3 +: absurdUnion)
            +: weaken4

class Member (u :: [InsClass] -> InsClass) f fs where
    injectRec :: f ~> u fs
    projectRec :: u fs a -> Maybe (f a)

instance
    ( SearchMemberRec fs u f fs found fihu
    , MemberFound f fs found
    , SingI fihu
    ) =>
    Member u f fs
    where
    injectRec = withFound @f @fs @found $ injectSMR @fs Refl sing
    projectRec = withFound @f @fs @found $ projectSMR @fs Refl sing
    {-# INLINE injectRec #-}
    {-# INLINE projectRec #-}

class MemberFound f fs found where
    withFound :: (found ~ 'True => a) -> a

instance MemberFound f fs 'True where
    withFound a = a
    {-# INLINE withFound #-}

-- A stopgap until upgrading to base-4.19.
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-TypeError.html#t:Unsatisfiable
instance
    TypeError
        ( 'Text "The effect class: " ':<>: 'ShowType f
            ':$$: 'Text " was not found in the union:"
            ':$$: 'Text "    " ':<>: 'ShowType fs
        ) =>
    MemberFound f fs 'False
    where
    withFound _ = error "unreachable"

type SearchMemberRec rest u f = SearchMemberRec_ (NextSearchMemberRecAction rest u f) rest u f

class
    SearchMemberRec_
        (act :: SearchMemberRecAction)
        (rest :: [Instruction])
        (u :: [Instruction] -> Instruction)
        (f :: Instruction)
        (fs :: [Instruction])
        (found :: Bool)
        (foundInHeadUnion :: Bool)
        | rest f -> found foundInHeadUnion
    where
    injectSMR_ :: found :~: 'True -> SBool foundInHeadUnion -> f ~> u fs
    projectSMR_ :: found :~: 'True -> SBool foundInHeadUnion -> u fs a -> Maybe (f a)

injectSMR ::
    forall rest u f fs found foundInHeadUnion.
    SearchMemberRec rest u f fs found foundInHeadUnion =>
    found :~: 'True ->
    SBool foundInHeadUnion ->
    f ~> u fs
injectSMR = injectSMR_ @(NextSearchMemberRecAction rest u f) @rest
{-# INLINE injectSMR #-}

projectSMR ::
    forall rest u f fs found foundInHeadUnion a.
    SearchMemberRec rest u f fs found foundInHeadUnion =>
    found :~: 'True ->
    SBool foundInHeadUnion ->
    u fs a ->
    Maybe (f a)
projectSMR = projectSMR_ @(NextSearchMemberRecAction rest u f) @rest
{-# INLINE projectSMR #-}

data SearchMemberRecAction = SmrStop | SmrRight | SmrDown

type family NextSearchMemberRecAction rest (u :: [InsClass] -> InsClass) f where
    NextSearchMemberRecAction (f ': _) u f = 'SmrStop
    NextSearchMemberRecAction (u _ ': _) u f = 'SmrDown
    NextSearchMemberRecAction _ u f = 'SmrRight

instance
    (HasMembership u f fs, Union u) =>
    SearchMemberRec_ 'SmrStop (f ': _tail) u f fs 'True 'False
    where
    injectSMR_ _ _ = inject
    projectSMR_ _ _ = project
    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

instance
    ( SearchMemberRec gs u f gs foundInHead fihuHead
    , If foundInHead (HasMembership u (u gs) fs) (() :: Constraint)
    , SearchMemberRec
        (If foundInHead '[] tail)
        u
        f
        fs
        foundInTail
        fihuTail
    , found ~ (foundInHead || foundInTail)
    , Union u
    , SingI fihuHead
    , SingI fihuTail
    ) =>
    SearchMemberRec_ 'SmrDown (u gs ': tail) u f fs found foundInHead
    where
    injectSMR_ Refl = \case
        STrue -> inject . injectSMR @gs @u @_ @gs Refl sing
        SFalse -> injectSMR @tail Refl sing

    projectSMR_ Refl = \case
        STrue -> projectSMR @gs @u @_ @gs Refl sing <=< project
        SFalse -> projectSMR @tail Refl sing

    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

instance
    (SearchMemberRec rest u f fs found fihu, SingI fihu) =>
    SearchMemberRec_ 'SmrRight (_f ': rest) u f fs found 'False
    where
    injectSMR_ refl _ = injectSMR @rest refl sing
    projectSMR_ refl _ = projectSMR @rest refl sing
    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}

instance SearchMemberRec_ act '[] u f fs 'False 'False where
    injectSMR_ = \case {}
    projectSMR_ = \case {}
    {-# INLINE injectSMR_ #-}
    {-# INLINE projectSMR_ #-}
