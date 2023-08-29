{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Sum where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.HFunctor (HFunctor, hmap)
import Control.Heftia (liftSig, translateH)
import Control.Heftia.Trans (TransHeftia, interpretT)
import Data.Free.Sum (NopF)
import Data.Hefty.Union (HFunctorUnion, Union, type (<:))
import Data.Hefty.Union qualified as U
import Data.Kind (Type)

infixr 6 +

data (h1 + h2) (f :: Type -> Type) a = L (h1 f a) | R (h2 f a)
    deriving (Functor, Foldable, Traversable)

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 + h2) where
    hmap f = \case
        L l -> L $ hmap f l
        R r -> R $ hmap f r

type Nop = LiftIns NopF

mergeHeftia ::
    forall h m sig sig' a c.
    (HFunctor sig, HFunctor sig', TransHeftia c h, c m) =>
    h (h m sig') sig a ->
    h m (sig + sig') a
mergeHeftia = interpretT (translateH @c R) (liftSig @c . L)

swapSum :: (h1 + h2) f a -> (h2 + h1) f a
swapSum = \case
    L x -> R x
    R x -> L x

type family Sum hs where
    Sum '[] = Nop
    Sum (h ': hs) = h + Sum hs

newtype SumUnion hs f a = SumUnion {unSumUnion :: Sum hs f a}

deriving instance Functor (SumUnion '[] f)
deriving instance (Functor (h f), Functor (Sum hs f)) => Functor (SumUnion (h ': hs) f)

deriving instance Foldable (SumUnion '[] f)
deriving instance (Foldable (h f), Foldable (Sum hs f)) => Foldable (SumUnion (h ': hs) f)

deriving instance Traversable (SumUnion '[] f)
deriving instance (Traversable (h f), Traversable (Sum hs f)) => Traversable (SumUnion (h ': hs) f)

deriving instance HFunctor (SumUnion '[])
deriving instance (HFunctor h, HFunctor (Sum hs)) => HFunctor (SumUnion (h ': hs))

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

instance h1 < (h1 + h2) where
    injH = L

    projH = \case
        L x -> Just x
        R _ -> Nothing

instance h1 < h3 => h1 < (h2 + h3) where
    injH = R . injH

    projH = \case
        L _ -> Nothing
        R x -> projH x
