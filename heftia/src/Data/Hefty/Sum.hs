{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Sum where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.HFunctor (HFunctor, hfmap)
import Data.Free.Sum (NopF)
import Data.Hefty.Union (HFunctorUnion, Union, type (<:))
import Data.Hefty.Union qualified as U
import Data.Kind (Type)

infixr 6 +

data (h1 + h2) (f :: Type -> Type) a = L (h1 f a) | R (h2 f a)
    deriving (Functor, Foldable, Traversable)

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 + h2) where
    hfmap f = \case
        L l -> L $ hfmap f l
        R r -> R $ hfmap f r

type Nop = LiftIns NopF

swapSum :: (h1 + h2) f a -> (h2 + h1) f a
swapSum = \case
    L x -> R x
    R x -> L x

type family Sum hs where
    Sum '[] = Nop
    Sum (h ': hs) = h + Sum hs

newtype SumUnion hs f a = SumUnion {unSumUnion :: Sum hs f a}

deriving newtype instance Functor (SumUnion '[] f)
deriving newtype instance (Functor (h f), Functor (Sum hs f)) => Functor (SumUnion (h ': hs) f)

deriving newtype instance Foldable (SumUnion '[] f)
deriving newtype instance (Foldable (h f), Foldable (Sum hs f)) => Foldable (SumUnion (h ': hs) f)

deriving stock instance Traversable (SumUnion '[] f)
deriving stock instance (Traversable (h f), Traversable (Sum hs f)) => Traversable (SumUnion (h ': hs) f)

deriving newtype instance HFunctor (Sum hs) => HFunctor (SumUnion hs)

instance Union SumUnion where
    type Member _ h hs = h < Sum hs

    inject sig = SumUnion $ injH sig
    project (SumUnion sig) = projH sig

    comp =
        SumUnion . \case
            Left x -> L x
            Right (SumUnion x) -> R x

    decomp (SumUnion sig) = case sig of
        L x -> Left x
        R x -> Right (SumUnion x)

instance HFunctor (SumUnion hs) => HFunctorUnion SumUnion hs

newtype ViaSumH (h :: Signature) f a = ViaSumH {getViaSumH :: h f a}
    deriving stock (Functor, Foldable, Traversable)

instance f < g => ViaSumH f <: g where
    weakenSig = injH . getViaSumH

class (h1 :: Signature) < h2 where
    injH :: h1 f a -> h2 f a
    projH :: h2 f a -> Maybe (h1 f a)

instance h < h where
    injH = id
    projH = Just

instance h1 < h2 => h1 < (h2 + h3) where
    injH = L . injH

    projH = \case
        L x -> projH x
        R _ -> Nothing
