{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia.Interpret.ViaTrans where

import Control.Applicative (Alternative)
import Control.Heftia (liftSig)
import Control.Heftia.Interpret (Bulk, Interpret, Surface, interpret, raise)
import Control.Heftia.Trans (TransHeftia, interpretR, liftLower)
import Control.Hefty (HFunctor, Signature, hmap)
import Control.Monad (MonadPlus)
import Data.Kind (Type)

newtype ViaHeftiaT c h f es (a :: Type) = ViaHeftiaT
    {runViaHeftiaT :: ExpandTrafoStack h f es a}

type family ExpandTrafoStack h (f :: Type -> Type) (es :: [Signature]) where
    ExpandTrafoStack h f '[] = f
    ExpandTrafoStack h f (e ': es) = h (ExpandTrafoStack h f es) e

deriving newtype instance Functor (ExpandTrafoStack h f es) => Functor (ViaHeftiaT c h f es)
deriving newtype instance Applicative (ExpandTrafoStack h f es) => Applicative (ViaHeftiaT c h f es)
deriving newtype instance Alternative (ExpandTrafoStack h f es) => Alternative (ViaHeftiaT c h f es)
deriving newtype instance Monad (ExpandTrafoStack h f es) => Monad (ViaHeftiaT c h f es)
deriving newtype instance MonadPlus (ExpandTrafoStack h f es) => MonadPlus (ViaHeftiaT c h f es)

class TransHeftia c h => Dive_ (flag :: Bool) e es c h f | h -> c where
    dive_ :: e (ExpandTrafoStack h f es) a -> ExpandTrafoStack h f es a

instance (TransHeftia c h, HFunctor e, c (ExpandTrafoStack h f es)) => Dive_ 'True e (e ': es) c h f where
    dive_ = liftSig @c

instance
    (TransHeftia c h, HFunctor e, HFunctor e', c (ExpandTrafoStack h f es), Dive e es c h f) =>
    Dive_ 'False e (e' ': es) c h f
    where
    dive_ = liftLower . dive @e @es @c @h @f . hmap undefined -- impossible?

type Dive e es = Dive_ (SigEq e (HeadSig es)) e es
dive :: forall e es c h f a. Dive e es c h f => e (ExpandTrafoStack h f es) a -> ExpandTrafoStack h f es a
dive = dive_ @(SigEq e (HeadSig es)) @e @es @c @h @f

type family HeadSig (es :: [Signature]) where
    HeadSig (e ': es) = e

type family SigEq (e :: Signature) e' where
    SigEq e e = 'True
    SigEq _ _ = 'False

instance TransHeftia c h => Interpret (ViaHeftiaT c h f) where
    type Surface _ e = HFunctor e
    type Bulk (ViaHeftiaT c h f) es = c (ExpandTrafoStack h f es)

    interpret i (ViaHeftiaT m) = ViaHeftiaT $ interpretR (runViaHeftiaT . i . hmap ViaHeftiaT) m

    raise = ViaHeftiaT . liftLower . runViaHeftiaT
