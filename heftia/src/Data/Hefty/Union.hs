{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Union where

import Control.Effect.Class (Signature)
import Data.Kind (Constraint)

class UnionH (u :: [Signature] -> Signature) where
    type HasMembershipH u (h :: Signature) (hs :: [Signature]) :: Constraint

    injectH :: HasMembershipH u h hs => h f a -> u hs f a
    projectH :: HasMembershipH u h hs => u hs f a -> Maybe (h f a)

    compH :: Either (h f a) (u hs f a) -> u (h ': hs) f a
    decompH :: u (h ': hs) f a -> Either (h f a) (u hs f a)

    absurdUnionH :: u '[] f a -> x

    infixr 5 |+:
    (|+:) :: (h f a -> r) -> (u hs f a -> r) -> u (h ': hs) f a -> r
    (f |+: g) u = case decompH u of
        Left x -> f x
        Right x -> g x

    weakenLH :: h f a -> u (h ': hs) f a
    weakenLH = compH . Left

    weakenRH :: u hs f a -> u (h ': hs) f a
    weakenRH = compH . Right

    {-# INLINE (|+:) #-}
    {-# INLINE weakenLH #-}
    {-# INLINE weakenRH #-}

-- So far, this seems unnecessary.
{-
type family IsMemberH (h :: Signature) hs where
    IsMemberH h (h ': hs) = 'True
    IsMemberH h (_ ': hs) = IsMemberH h hs
    IsMemberH _ '[] = 'False
-}

type MemberH u h hs = ({-IsMemberH h hs ~ 'True, -} HasMembershipH u h hs)
