{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Free.Sum (module Data.Free.Sum, pattern L1, pattern R1)
where

import Control.Effect.Class (Instruction)
import Data.Free.Union (Member, Union, absurdUnion, comp, decomp, inject, project)
import Data.Kind (Type)
import GHC.Generics (type (:+:) (L1, R1))

infixr 6 +

-- | A type synonym for disambiguation to the sum on the higher-order side.
type (+) = (:+:)

data NopF (a :: Type)
    deriving (Functor, Foldable, Traversable)

swapSum :: (f :+: g) a -> (g :+: f) a
swapSum = \case
    L1 x -> R1 x
    R1 x -> L1 x
{-# INLINE swapSum #-}

type family Sum fs where
    Sum '[] = NopF
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

newtype ViaSum (f :: Instruction) a = ViaSum {getViaSum :: f a}
    deriving stock (Functor, Foldable, Traversable)

class (f :: Instruction) < g where
    inj :: f a -> g a
    proj :: g a -> Maybe (f a)

instance h < h where
    inj = id
    proj = Just

    {-# INLINE inj #-}
    {-# INLINE proj #-}

instance h1 < h2 => h1 < (h2 :+: h3) where
    inj = L1 . inj

    proj = \case
        L1 x -> proj x
        R1 _ -> Nothing

    {-# INLINE inj #-}
    {-# INLINE proj #-}
