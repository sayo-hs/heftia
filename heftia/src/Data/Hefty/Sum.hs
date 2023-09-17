{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- The code before modification is MIT licensed; (c) 2023 Casper Bach Poulsen and Cas van der Rest.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

An implementation of an open union for higher-order effects using recursively nested binary sums.
-}
module Data.Hefty.Sum where

import Control.Effect.Class (NopS, Signature, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, caseH, (:+:) (Inl, Inr))
import Data.Hefty.Union (HasMembershipH, UnionH, absurdUnionH, compH, decompH, injectH, projectH)

absurdLH :: (NopS :+: h) f ~> h f
absurdLH = caseH \case {} id
{-# INLINE absurdLH #-}

absurdRH :: (h :+: NopS) f ~> h f
absurdRH = caseH id \case {}
{-# INLINE absurdRH #-}

swapSumH :: (h1 :+: h2) f a -> (h2 :+: h1) f a
swapSumH = caseH Inr Inl
{-# INLINE swapSumH #-}

type family SumH hs where
    SumH '[] = NopS
    SumH (h ': hs) = h :+: SumH hs

{- |
An implementation of an open union for higher-order effects using recursively nested binary sums.
-}
newtype SumUnionH hs f a = SumUnionH {unSumUnionH :: SumH hs f a}

deriving newtype instance Functor (SumUnionH '[] f)
deriving newtype instance Foldable (SumUnionH '[] f)
deriving stock instance Traversable (SumUnionH '[] f)

{- Lack of instances of 'Data.Comp.Multi.Ops.:+:'.
 - Should we create a pullreq on the compdata package side?
 -}
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
    type HasMembershipH _ h hs = h << SumH hs

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

class isHead ~ h1 `IsHeadSigOf` h2 => SumMemberH isHead (h1 :: Signature) h2 where
    injSumH :: h1 f a -> h2 f a
    projSumH :: h2 f a -> Maybe (h1 f a)

type family (h1 :: Signature) `IsHeadSigOf` h2 where
    f `IsHeadSigOf` f :+: g = 'True
    _ `IsHeadSigOf` _ = 'False

type h1 << h2 = SumMemberH (IsHeadSigOf h1 h2) h1 h2

injH :: forall h1 h2 f. h1 << h2 => h1 f ~> h2 f
injH = injSumH @(IsHeadSigOf h1 h2)

projH :: forall h1 h2 f a. h1 << h2 => h2 f a -> Maybe (h1 f a)
projH = projSumH

instance SumMemberH 'True f (f :+: g) where
    injSumH = Inl

    projSumH = \case
        Inl x -> Just x
        Inr _ -> Nothing

    {-# INLINE injSumH #-}
    {-# INLINE projSumH #-}

instance (f `IsHeadSigOf` (g :+: h) ~ 'False, f << h) => SumMemberH 'False f (g :+: h) where
    injSumH = Inr . injH
    projSumH = \case
        Inl _ -> Nothing
        Inr x -> projSumH x

    {-# INLINE injSumH #-}
    {-# INLINE projSumH #-}
