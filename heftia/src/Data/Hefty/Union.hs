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

A type class representing a general open union for higher-order effects, independent of the internal
implementation.
-}
module Data.Hefty.Union where

import Control.Effect.Class (Signature, type (~>))
import Control.Effect.Class.Machinery.DepParams (
    DepParams,
    DepParamsOfH,
    EffectClassIdentifier,
    EffectClassIdentifierOfH,
    SigClassOf,
 )
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

{- |
A type class representing a general open union for higher-order effects, independent of the internal
implementation.
-}
class UnionH (u :: [Signature] -> Signature) where
    {-# MINIMAL injectH, projectH, absurdUnionH, (compH | (inject0H, weakenH), decompH | (|+:)) #-}

    type HasMembershipH u (h :: Signature) (hs :: [Signature]) :: Constraint

    injectH :: HasMembershipH u h hs => h f ~> u hs f
    projectH :: HasMembershipH u h hs => u hs f a -> Maybe (h f a)

    absurdUnionH :: u '[] f a -> x

    compH :: Either (h f a) (u hs f a) -> u (h ': hs) f a
    compH = \case
        Left x -> inject0H x
        Right x -> weakenH x
    {-# INLINE compH #-}

    decompH :: u (h ': hs) f a -> Either (h f a) (u hs f a)
    decompH = Left |+: Right
    {-# INLINE decompH #-}

    infixr 5 |+:
    (|+:) :: (h f a -> r) -> (u hs f a -> r) -> u (h ': hs) f a -> r
    (f |+: g) u = case decompH u of
        Left x -> f x
        Right x -> g x
    {-# INLINE (|+:) #-}

    inject0H :: h f ~> u (h ': hs) f
    inject0H = compH . Left
    {-# INLINE inject0H #-}

    injectUnderH :: h2 f ~> u (h1 ': h2 ': hs) f
    injectUnderH = weakenH . inject0H
    {-# INLINE injectUnderH #-}

    injectUnder2H :: h3 f ~> u (h1 ': h2 ': h3 ': hs) f
    injectUnder2H = weaken2H . inject0H
    {-# INLINE injectUnder2H #-}

    injectUnder3H :: h4 f ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    injectUnder3H = weaken3H . inject0H
    {-# INLINE injectUnder3H #-}

    weakenH :: u hs f ~> u (h ': hs) f
    weakenH = compH . Right
    {-# INLINE weakenH #-}

    weaken2H :: u hs f ~> u (h1 ': h2 ': hs) f
    weaken2H = weakenH . weakenH
    {-# INLINE weaken2H #-}

    weaken3H :: u hs f ~> u (h1 ': h2 ': h3 ': hs) f
    weaken3H = weaken2H . weakenH
    {-# INLINE weaken3H #-}

    weaken4H :: u hs f ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    weaken4H = weaken3H . weakenH
    {-# INLINE weaken4H #-}

    weakenUnderH :: u (h1 ': hs) f ~> u (h1 ': h2 ': hs) f
    weakenUnderH = inject0H |+: weaken2H

    weakenUnder2H :: u (h1 ': h2 ': hs) f ~> u (h1 ': h2 ': h3 ': hs) f
    weakenUnder2H = inject0H |+: injectUnderH |+: weaken3H

    weakenUnder3H :: u (h1 ': h2 ': h3 ': hs) f ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    weakenUnder3H = inject0H |+: injectUnderH |+: injectUnder2H |+: weaken4H

    weaken2UnderH :: u (h1 ': hs) f ~> u (h1 ': h2 ': h3 ': hs) f
    weaken2UnderH = inject0H |+: weaken3H

    weaken2Under2H :: u (h1 ': h2 ': hs) f ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    weaken2Under2H = inject0H |+: injectUnderH |+: weaken4H

    weaken3UnderH :: u (h1 ': hs) f ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    weaken3UnderH = inject0H |+: weaken4H

    flipUnionH :: u (h1 ': h2 ': hs) f ~> u (h2 ': h1 ': hs) f
    flipUnionH = injectUnderH |+: inject0H |+: weaken2H

    flipUnion3H :: u (h1 ': h2 ': h3 ': hs) f ~> u (h3 ': h2 ': h1 ': hs) f
    flipUnion3H = injectUnder2H |+: injectUnderH |+: inject0H |+: weaken3H

    flipUnionUnderH :: u (h1 ': h2 ': h3 ': hs) f ~> u (h1 ': h3 ': h2 ': hs) f
    flipUnionUnderH = inject0H |+: injectUnder2H |+: injectUnderH |+: weaken3H

    rot3H :: u (h1 ': h2 ': h3 ': hs) f ~> u (h2 ': h3 ': h1 ': hs) f
    rot3H = injectUnder2H |+: inject0H |+: injectUnderH |+: weaken3H

    rot3H' :: u (h1 ': h2 ': h3 ': hs) f ~> u (h3 ': h1 ': h2 ': hs) f
    rot3H' = injectUnderH |+: injectUnder2H |+: inject0H |+: weaken3H

    bundleUnion2H :: UnionH u' => u (h1 ': h2 ': hs) f ~> u (u' '[h1, h2] ': hs) f
    bundleUnion2H = inject0H . inject0H |+: inject0H . injectUnderH |+: weakenH

    bundleUnion3H :: UnionH u' => u (h1 ': h2 ': h3 ': hs) f ~> u (u' '[h1, h2, h3] ': hs) f
    bundleUnion3H =
        (inject0H . inject0H)
            |+: (inject0H . injectUnderH)
            |+: (inject0H . injectUnder2H)
            |+: weakenH

    bundleUnion4H ::
        UnionH u' =>
        u (h1 ': h2 ': h3 ': h4 ': hs) f ~> u (u' '[h1, h2, h3, h4] ': hs) f
    bundleUnion4H =
        (inject0H . inject0H)
            |+: (inject0H . injectUnderH)
            |+: (inject0H . injectUnder2H)
            |+: (inject0H . injectUnder3H)
            |+: weakenH

    unbundleUnion2H :: UnionH u' => u (u' '[h1, h2] ': hs) f ~> u (h1 ': h2 ': hs) f
    unbundleUnion2H = (inject0H |+: injectUnderH |+: absurdUnionH) |+: weaken2H

    unbundleUnion3H :: UnionH u' => u (u' '[h1, h2, h3] ': hs) f ~> u (h1 ': h2 ': h3 ': hs) f
    unbundleUnion3H = (inject0H |+: injectUnderH |+: injectUnder2H |+: absurdUnionH) |+: weaken3H

    unbundleUnion4H ::
        UnionH u' =>
        u (u' '[h1, h2, h3, h4] ': hs) f
            ~> u (h1 ': h2 ': h3 ': h4 ': hs) f
    unbundleUnion4H =
        (inject0H |+: injectUnderH |+: injectUnder2H |+: injectUnder3H |+: absurdUnionH)
            |+: weaken4H

type family IsMemberH (h :: Signature) hs where
    IsMemberH h (h ': hs) = 'True
    IsMemberH h (_ ': hs) = IsMemberH h hs
    IsMemberH _ '[] = 'False

type family CheckMemberH isMember h hs :: Constraint where
    CheckMemberH 'True h hs = ()
    CheckMemberH 'False h hs =
        TypeError
            ( 'Text "The signature class: " ':<>: 'ShowType h
                ':$$: 'Text " was not found in the list:"
                ':$$: 'Text "    " ':<>: 'ShowType hs
            )

type MemberH u h hs = (CheckMemberH (IsMemberH h hs) h hs, HasMembershipH u h hs, IsMemberH h hs ~ 'True)

type FirstDepParamsH eci es f = FindFirstDepParamsH es eci
type MemberDepH u eci es = MemberH u (SigClassIn es eci) es
type SigClassIn es eci = SigClassOf eci (FirstDepParamsInH es eci :: DepParams eci)
type FirstDepParamsInH es eci = FromJustFirstDepParamsH (FindFirstDepParamsH es eci) es eci

type family FindFirstDepParamsH (es :: [Signature]) (eci :: EffectClassIdentifier) where
    FindFirstDepParamsH (e ': es) eci =
        MatchEffectClassH eci (EffectClassIdentifierOfH e) (DepParamsOfH e) es
    FindFirstDepParamsH '[] eci = 'Nothing

type family MatchEffectClassH eci0 eci1 dps es where
    MatchEffectClassH eci eci dps _ = 'Just dps
    MatchEffectClassH eci _ _ es = FindFirstDepParamsH es eci

type family
    FromJustFirstDepParamsH
        (mDPS :: Maybe k)
        (es :: [Signature])
        (eci :: EffectClassIdentifier)
    where
    FromJustFirstDepParamsH ('Just dps) _ _ = dps
    FromJustFirstDepParamsH 'Nothing es eci =
        TypeError
            ( 'Text "No signature class matching the effect class identifier:"
                ':$$: 'Text "    " ':<>: 'ShowType eci
                ':$$: 'Text " was found in the list:"
                ':$$: 'Text "    " ':<>: 'ShowType es
            )
