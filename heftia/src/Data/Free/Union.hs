-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Effect.Class.Machinery.DepParams (
    DepParams,
    DepParamsOf,
    EffectClassIdentifier,
    EffectClassIdentifierOf,
    InsClassOf,
    QueryDepParamsFor,
 )
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

{- |
A type class representing a general open union for first-order effects, independent of the internal
implementation.
-}
class Union (u :: [Instruction] -> Instruction) where
    {-# MINIMAL inject, project, absurdUnion, (comp | (inject0, weaken), decomp | (|+|:)) #-}

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
    decomp = Left |+|: Right
    {-# INLINE decomp #-}

    infixr 5 |+|:
    (|+|:) :: (f a -> r) -> (u fs a -> r) -> u (f ': fs) a -> r
    (f |+|: g) u = case decomp u of
        Left x -> f x
        Right x -> g x
    {-# INLINE (|+|:) #-}

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
    weakenUnder = inject0 |+|: weaken2

    weakenUnder2 :: u (f1 ': f2 ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    weakenUnder2 = inject0 |+|: injectUnder |+|: weaken3

    weakenUnder3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weakenUnder3 = inject0 |+|: injectUnder |+|: injectUnder2 |+|: weaken4

    weaken2Under :: u (f1 ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    weaken2Under = inject0 |+|: weaken3

    weaken2Under2 :: u (f1 ': f2 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weaken2Under2 = inject0 |+|: injectUnder |+|: weaken4

    weaken3Under :: u (f1 ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    weaken3Under = inject0 |+|: weaken4

    flipUnion :: u (f1 ': f2 ': fs) ~> u (f2 ': f1 ': fs)
    flipUnion = injectUnder |+|: inject0 |+|: weaken2

    flipUnion3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f3 ': f2 ': f1 ': fs)
    flipUnion3 = injectUnder2 |+|: injectUnder |+|: inject0 |+|: weaken3

    flipUnionUnder :: u (f1 ': f2 ': f3 ': fs) ~> u (f1 ': f3 ': f2 ': fs)
    flipUnionUnder = inject0 |+|: injectUnder2 |+|: injectUnder |+|: weaken3

    rot3 :: u (f1 ': f2 ': f3 ': fs) ~> u (f2 ': f3 ': f1 ': fs)
    rot3 = injectUnder2 |+|: inject0 |+|: injectUnder |+|: weaken3

    rot3' :: u (f1 ': f2 ': f3 ': fs) ~> u (f3 ': f1 ': f2 ': fs)
    rot3' = injectUnder |+|: injectUnder2 |+|: inject0 |+|: weaken3

    bundleUnion2 :: Union u' => u (f1 ': f2 ': fs) ~> u (u' '[f1, f2] ': fs)
    bundleUnion2 = inject0 . inject0 |+|: inject0 . injectUnder |+|: weaken

    bundleUnion3 :: Union u' => u (f1 ': f2 ': f3 ': fs) ~> u (u' '[f1, f2, f3] ': fs)
    bundleUnion3 =
        (inject0 . inject0)
            |+|: (inject0 . injectUnder)
            |+|: (inject0 . injectUnder2)
            |+|: weaken

    bundleUnion4 :: Union u' => u (f1 ': f2 ': f3 ': f4 ': fs) ~> u (u' '[f1, f2, f3, f4] ': fs)
    bundleUnion4 =
        (inject0 . inject0)
            |+|: (inject0 . injectUnder)
            |+|: (inject0 . injectUnder2)
            |+|: (inject0 . injectUnder3)
            |+|: weaken

    unbundleUnion2 :: Union u' => u (u' '[f1, f2] ': fs) ~> u (f1 ': f2 ': fs)
    unbundleUnion2 = (inject0 |+|: injectUnder |+|: absurdUnion) |+|: weaken2

    unbundleUnion3 :: Union u' => u (u' '[f1, f2, f3] ': fs) ~> u (f1 ': f2 ': f3 ': fs)
    unbundleUnion3 = (inject0 |+|: injectUnder |+|: injectUnder2 |+|: absurdUnion) |+|: weaken3

    unbundleUnion4 :: Union u' => u (u' '[f1, f2, f3, f4] ': fs) ~> u (f1 ': f2 ': f3 ': f4 ': fs)
    unbundleUnion4 =
        (inject0 |+|: injectUnder |+|: injectUnder2 |+|: injectUnder3 |+|: absurdUnion)
            |+|: weaken4

type family IsMember (f :: Instruction) fs where
    IsMember f (f ': fs) = 'True
    IsMember f (_ ': fs) = IsMember f fs
    IsMember _ '[] = 'False

type family CheckMember isMember f fs :: Constraint where
    CheckMember 'True f fs = ()
    CheckMember 'False f fs =
        TypeError
            ( 'Text "The instruction class: " ':<>: 'ShowType f
                ':$$: 'Text " was not found in the list:"
                ':$$: 'Text "    " ':<>: 'ShowType fs
            )

type Member u f fs = (CheckMember (IsMember f fs) f fs, HasMembership u f fs, IsMember f fs ~ 'True)

type FirstDepParams eci es f = FindFirstDepParams es eci `ThenQueryDepParamsFor` '(eci, f)
type MemberDep u eci es = Member u (InsClassIn es eci) es
type InsClassIn es eci = InsClassOf eci (FirstDepParamsIn es eci :: DepParams eci)
type FirstDepParamsIn es eci = FromJustFirstDepParams (FindFirstDepParams es eci) es eci

type family FindFirstDepParams (es :: [Instruction]) (eci :: EffectClassIdentifier) where
    FindFirstDepParams (e ': es) eci =
        MatchEffectClass eci (EffectClassIdentifierOf e) (DepParamsOf e) es
    FindFirstDepParams '[] eci = 'Nothing

type family MatchEffectClass eci0 eci1 dps es where
    MatchEffectClass eci eci dps _ = 'Just dps
    MatchEffectClass eci _ _ es = FindFirstDepParams es eci

type family
    ThenQueryDepParamsFor
        (mDPS :: Maybe k)
        (eci'f :: (EffectClassIdentifier, Type -> Type))
    where
    ThenQueryDepParamsFor ('Just dps) _ = 'Just dps
    ThenQueryDepParamsFor 'Nothing '(eci, f) = QueryDepParamsFor eci f

type family
    FromJustFirstDepParams
        (mDPS :: Maybe k)
        (es :: [Instruction])
        (eci :: EffectClassIdentifier)
    where
    FromJustFirstDepParams ('Just dps) _ _ = dps
    FromJustFirstDepParams 'Nothing es eci =
        TypeError
            ( 'Text "No instruction class matching the effect class identifier:"
                ':$$: 'Text "    " ':<>: 'ShowType eci
                ':$$: 'Text " was found in the list:"
                ':$$: 'Text "    " ':<>: 'ShowType es
            )
