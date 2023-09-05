{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Sum where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, caseH, (:+:) (Inl, Inr))
import Data.Free.Sum (NopF)
import Data.Hefty.Union (MemberH, UnionH, absurdUnionH, compH, decompH, injectH, projectH, weakenSig, type (<:))

type Nop = LiftIns NopF

swapSumH :: (h1 :+: h2) f a -> (h2 :+: h1) f a
swapSumH = caseH Inr Inl
{-# INLINE swapSumH #-}

type family SumH hs where
    SumH '[] = Nop
    SumH (h ': hs) = h :+: SumH hs

newtype SumUnionH hs f a = SumUnionH {unSumUnionH :: SumH hs f a}

deriving newtype instance Functor (SumUnionH '[] f)
deriving newtype instance Foldable (SumUnionH '[] f)
deriving stock instance Traversable (SumUnionH '[] f)

{-
deriving newtype instance
    (Functor (h f), Functor (SumH hs f)) =>
    Functor (SumUnionH (h ': hs) f)

deriving newtype instance
    (Foldable (h f), Foldable (SumH hs f)) =>
    Foldable (SumUnionH (h ': hs) f)

deriving stock instance
    (Traversable (h f), Traversable (SumH hs f)) =>
    Traversable (SumUnionH (h ': hs) f)
-}

deriving newtype instance HFunctor (SumH hs) => HFunctor (SumUnionH hs)

instance UnionH SumUnionH where
    type MemberH _ h hs = h << SumH hs

    injectH sig = SumUnionH $ injH sig
    projectH (SumUnionH sig) = projH sig

    absurdUnionH = \case {}

    compH =
        SumUnionH . \case
            Left x -> Inl x
            Right (SumUnionH x) -> Inr x

    decompH (SumUnionH sig) = case sig of
        Inl x -> Left x
        Inr x -> Right (SumUnionH x)

    {-# INLINE injectH #-}
    {-# INLINE projectH #-}
    {-# INLINE absurdUnionH #-}

newtype ViaSumH (h :: Signature) f a = ViaSumH {getViaSumH :: h f a}
    deriving stock (Functor, Foldable, Traversable)

instance f << g => ViaSumH f <: g where
    weakenSig = injH . getViaSumH

class (h1 :: Signature) << h2 where
    injH :: h1 f a -> h2 f a
    projH :: h2 f a -> Maybe (h1 f a)

instance h << h where
    injH = id
    projH = Just

    {-# INLINE injH #-}
    {-# INLINE projH #-}

instance h1 << h2 => h1 << (h2 :+: h3) where
    injH = Inl . injH

    projH = \case
        Inl x -> projH x
        Inr _ -> Nothing

    {-# INLINE injH #-}
    {-# INLINE projH #-}
