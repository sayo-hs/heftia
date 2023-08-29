{-# LANGUAGE UndecidableInstances #-}

module Data.Hefty.Union where

import Control.Effect.Class (Signature)
import Control.Effect.Class.HFunctor (HFunctor)
import Control.Natural (type (~>))
import Data.Kind (Constraint)

class Union (u :: [Signature] -> Signature) where
    type Member u (h :: Signature) (hs :: [Signature]) :: Constraint

    inject :: Member u h hs => h f a -> u hs f a
    project :: Member u h hs => u hs f a -> Maybe (h f a)

    comp :: Either (h f a) (u hs f a) -> u (h ': hs) f a
    decomp :: u (h ': hs) f a -> Either (h f a) (u hs f a)

    weakenL :: h f a -> u (h ': hs) f a
    weakenL = comp . Left

    weakenR :: u hs f a -> u (h ': hs) f a
    weakenR = comp . Right

class (Union u, HFunctor (u hs)) => HFunctorUnion u hs

class s <: t where
    weakenSig :: s m ~> t m

newtype ViaUnion (u :: [Signature] -> Signature) (h :: Signature) f a = ViaUnion {getViaUnion :: h f a}
    deriving stock (Functor, Foldable, Traversable)

instance (Union u, Member u h hs) => ViaUnion u h <: u hs where
    weakenSig = inject . getViaUnion

newtype ViaSingleton (h :: Signature) f a = ViaSingleton {getViaSingleton :: h f a}
    deriving stock (Functor, Foldable, Traversable)

instance ViaSingleton h <: h where
    weakenSig = getViaSingleton
