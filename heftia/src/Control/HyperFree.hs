{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.HyperFree where

import Control.Applicative (Alternative)
import Control.Effect.Class (type (~>))
import Control.Effect.Free (InsClass)
import Control.Freer (Freer, transformFreer)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

newtype
    HyperFree
        (f :: InsClass -> Type -> Type)
        (e :: ASigClass)
        (a :: Type) = HyperFree
    {unHyperFree :: (f :# GetSigClass e) a}

type SigClass = (ASigClass -> Type -> Type) -> ASigClass -> Type -> Type
newtype ASigClass = ASigClass SigClass

type family GetSigClass (a :: ASigClass) where
    GetSigClass ('ASigClass e) = e

infixr 4 #, :#
type f # e = f ('ASigClass e)
type f :# e = f (e (HyperFree f) # e)

deriving newtype instance Functor (f :# e) => Functor (HyperFree f ('ASigClass e))
deriving newtype instance Applicative (f :# e) => Applicative (HyperFree f ('ASigClass e))
deriving newtype instance Alternative (f :# e) => Alternative (HyperFree f ('ASigClass e))
deriving newtype instance Monad (f :# e) => Monad (HyperFree f ('ASigClass e))
deriving newtype instance MonadPlus (f :# e) => MonadPlus (HyperFree f ('ASigClass e))
deriving newtype instance (MonadBase b (f :# e), Monad b) => MonadBase b (HyperFree f ('ASigClass e))
deriving newtype instance MonadIO (f :# e) => MonadIO (HyperFree f ('ASigClass e))
deriving newtype instance MonadFail (f :# e) => MonadFail (HyperFree f ('ASigClass e))

deriving stock instance Foldable (f :# e) => Foldable (HyperFree f ('ASigClass e))
deriving stock instance Traversable (f :# e) => Traversable (HyperFree f ('ASigClass e))

deriving newtype instance Eq ((f :# e) a) => Eq (HyperFree f ('ASigClass e) a)
deriving newtype instance Ord ((f :# e) a) => Ord (HyperFree f ('ASigClass e) a)
deriving newtype instance Read ((f :# e) a) => Read (HyperFree f ('ASigClass e) a)
deriving newtype instance Show ((f :# e) a) => Show (HyperFree f ('ASigClass e) a)

overHyperFree ::
    ((f :# GetSigClass e) a -> (f' :# GetSigClass e') b) ->
    HyperFree f e a ->
    HyperFree f' e' b
overHyperFree f = HyperFree . f . unHyperFree
{-# INLINE overHyperFree #-}

newtype
    LiftIns
        (e :: InsClass)
        (h :: ASigClass -> Type -> Type)
        (eh :: ASigClass)
        (a :: Type) = LiftIns {unliftIns :: e a}
    deriving stock (Functor, Foldable, Traversable)

class HyperFunctor (h :: ASigClass -> Type -> Type) where
    hyfmap :: ((e1 h # e1) ~> (e2 h # e2)) -> (h # e1) ~> (h # e2)

instance Freer c f => HyperFunctor (HyperFree f) where
    hyfmap f (HyperFree a) = HyperFree $ transformFreer f a
    {-# INLINE hyfmap #-}
