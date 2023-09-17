{-# LANGUAGE PatternSynonyms #-}
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

An implementation of an open union for first-order effects using recursively nested binary sums.
-}
module Data.Free.Sum (module Data.Free.Sum, pattern L1, pattern R1) where

import Control.Effect.Class (Instruction, NopI, type (~>))
import Data.Free.Union (HasMembership, Union, absurdUnion, comp, decomp, inject, project)
import GHC.Generics (type (:+:) (L1, R1))

infixr 6 +

-- | A type synonym for disambiguation to the sum on the higher-order side.
type (+) = (:+:)

caseF :: (f a -> r) -> (g a -> r) -> (f + g) a -> r
caseF f g = \case
    L1 x -> f x
    R1 x -> g x
{-# INLINE caseF #-}

absurdL :: (NopI + f) ~> f
absurdL = caseF \case {} id
{-# INLINE absurdL #-}

absurdR :: (f + NopI) ~> f
absurdR = caseF id \case {}
{-# INLINE absurdR #-}

swapSum :: (f + g) a -> (g + f) a
swapSum = caseF R1 L1
{-# INLINE swapSum #-}

type family Sum fs where
    Sum '[] = NopI
    Sum (f ': fs) = f :+: Sum fs

{- |
An implementation of an open union for first-order effects using recursively nested binary sums.
-}
newtype SumUnion fs a = SumUnion {unSumUnion :: Sum fs a}

deriving newtype instance Functor (SumUnion '[])
deriving newtype instance (Functor f, Functor (Sum fs)) => Functor (SumUnion (f ': fs))

deriving newtype instance Foldable (SumUnion '[])
deriving newtype instance (Foldable f, Foldable (Sum fs)) => Foldable (SumUnion (f ': fs))

deriving stock instance Traversable (SumUnion '[])
deriving stock instance (Traversable f, Traversable (Sum fs)) => Traversable (SumUnion (f ': fs))

instance Union SumUnion where
    type HasMembership _ f fs = f < Sum fs

    inject sig = SumUnion $ inj sig
    project (SumUnion sig) = proj sig

    absurdUnion = \case {}

    comp =
        SumUnion . \case
            Left x -> L1 x
            Right (SumUnion x) -> R1 x

    decomp (SumUnion sig) = case sig of
        L1 x -> Left x
        R1 x -> Right (SumUnion x)

    {-# INLINE inject #-}
    {-# INLINE project #-}
    {-# INLINE absurdUnion #-}

class isHead ~ f `IsHeadInsOf` g => SumMember isHead (f :: Instruction) g where
    injSum :: f a -> g a
    projSum :: g a -> Maybe (f a)

type family (f :: Instruction) `IsHeadInsOf` g where
    f `IsHeadInsOf` f + g = 'True
    _ `IsHeadInsOf` _ = 'False

type f < g = SumMember (IsHeadInsOf f g) f g

inj :: forall f g. f < g => f ~> g
inj = injSum @(IsHeadInsOf f g)

proj :: forall f g a. f < g => g a -> Maybe (f a)
proj = projSum

instance SumMember 'True f (f + g) where
    injSum = L1

    projSum = \case
        L1 x -> Just x
        R1 _ -> Nothing

    {-# INLINE injSum #-}
    {-# INLINE projSum #-}

instance (f `IsHeadInsOf` (g + h) ~ 'False, f < h) => SumMember 'False f (g + h) where
    injSum = R1 . inj
    projSum = \case
        L1 _ -> Nothing
        R1 x -> projSum x

    {-# INLINE injSum #-}
    {-# INLINE projSum #-}
