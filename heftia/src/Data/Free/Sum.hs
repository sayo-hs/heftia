{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- The code before modification is MIT licensed; (c) 2023 Casper Bach Poulsen and Cas van der Rest.

module Data.Free.Sum (module Data.Free.Sum, pattern L1, pattern R1)
where

import Control.Effect.Class (Instruction, NopI, type (~>))
import Data.Free.Union (Member, Union, absurdUnion, comp, decomp, inject, project)
import GHC.Generics (type (:+:) (L1, R1))

infixr 6 +

-- | A type synonym for disambiguation to the sum on the higher-order side.
type (+) = (:+:)

swapSum :: (f + g) a -> (g + f) a
swapSum = \case
    L1 x -> R1 x
    R1 x -> L1 x
{-# INLINE swapSum #-}

type family Sum fs where
    Sum '[] = NopI
    Sum (f ': fs) = f :+: Sum fs

newtype SumUnion fs a = SumUnion {unSumUnion :: Sum fs a}

deriving newtype instance Functor (SumUnion '[])
deriving newtype instance (Functor f, Functor (Sum fs)) => Functor (SumUnion (f ': fs))

deriving newtype instance Foldable (SumUnion '[])
deriving newtype instance (Foldable f, Foldable (Sum fs)) => Foldable (SumUnion (f ': fs))

deriving stock instance Traversable (SumUnion '[])
deriving stock instance (Traversable f, Traversable (Sum fs)) => Traversable (SumUnion (f ': fs))

instance Union SumUnion where
    type Member _ f fs = f < Sum fs

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

type family f `IsHeadInsOf` g where
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
