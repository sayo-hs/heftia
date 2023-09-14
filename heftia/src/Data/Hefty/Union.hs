{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Union where

import Control.Effect.Class (Signature, type (~>))
import Data.Kind (Constraint)

class UnionH (u :: [Signature] -> Signature) where
    type HasMembershipH u (h :: Signature) (hs :: [Signature]) :: Constraint

    injectH :: HasMembershipH u h hs => h f ~> u hs f
    projectH :: HasMembershipH u h hs => u hs f a -> Maybe (h f a)

    compH :: Either (h f a) (u hs f a) -> u (h ': hs) f a
    decompH :: u (h ': hs) f a -> Either (h f a) (u hs f a)

    absurdUnionH :: u '[] f a -> x

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

type MemberH u h hs = (HasMembershipH u h hs, IsMemberH h hs ~ 'True)
